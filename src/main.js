import 'leaflet/dist/leaflet.css';
import './style.css';
import L from 'leaflet';
import {
  Chart, BarController, BarElement, LineController, LineElement, PointElement,
  CategoryScale, LinearScale, Tooltip, Legend,
} from 'chart.js';
import pkg from '../package.json';
import { loadMeta, loadTrends, loadPopulation, loadSections, loadProvinces } from './data.js';
import {
  TOTAL, indexSections, rowValue, rowPercent, byMunicipality, measure,
  provinceAggregate, nationalAggregate, quantileBreaks, classOf,
} from './stats.js';
import { buildKml } from './kml.js';
import { findFeature } from './geo.js';
import { Picker } from './picker.js';
import { currentPosition, saveTextFile } from './native.js';

Chart.register(BarController, BarElement, LineController, LineElement, PointElement, CategoryScale, LinearScale, Tooltip, Legend);

const $ = (id) => document.getElementById(id);
const nf = new Intl.NumberFormat('es-ES');
const pf = new Intl.NumberFormat('es-ES', { minimumFractionDigits: 2, maximumFractionDigits: 2 });
const fmt = (v, percent) => (v === null || v === undefined ? 'sin datos' : percent ? `${pf.format(v)} %` : nf.format(Math.round(v)));
const SEXES = [['both', 'Ambos sexos'], ['men', 'Hombres'], ['women', 'Mujeres']];
const DEFAULT_PROVINCE = '30';

const state = { year: 2021, view: 'sections' };
let meta;
let trends;

// ---------- theme ----------
const css = (name) => getComputedStyle(document.documentElement).getPropertyValue(name).trim();
const dark = window.matchMedia('(prefers-color-scheme: dark)');
const seqColors = () => [1, 2, 3, 4, 5].map((i) => css(`--seq-${i}`));
const seriesColors = () => [1, 2, 3, 4, 5, 6, 7, 8].map((i) => css(`--series-${i}`));

// Base map: IGN (Instituto Geográfico Nacional) open WMTS, CC BY 4.0. One layer for both themes;
// the dark theme inverts it in CSS (.basemap).
function tileLayer() {
  return L.tileLayer(
    'https://www.ign.es/wmts/ign-base?service=WMTS&request=GetTile&version=1.0.0&format=image/png' +
      '&layer=IGNBaseTodo&style=default&tilematrixset=GoogleMapsCompatible&tilematrix={z}&tilerow={y}&tilecol={x}',
    {
      className: 'basemap',
      maxZoom: 19,
      attribution: '<a href="https://www.scne.es/">IGN</a> CC BY 4.0 · Datos: <a href="https://www.ine.es/">INE</a>',
    },
  );
}

function makeMap(el) {
  const map = L.map(el, { preferCanvas: true, zoomSnap: 0.5 });
  map.setView([40.2, -3.7], 6);
  tileLayer().addTo(map);
  return map;
}

function legend(el, breaks, percent) {
  const colors = seqColors();
  if (!breaks.length) {
    el.innerHTML = '';
    return;
  }
  let lo = null;
  const rows = breaks.map((b, i) => {
    const from = lo === null ? null : lo;
    lo = b;
    const label = from === null ? `hasta ${fmt(b, percent)}` : `${fmt(from, percent)} – ${fmt(b, percent)}`;
    return `<li><i style="background:${colors[colorSlot(i, breaks.length)]}"></i>${label}</li>`;
  });
  rows.push(`<li><i class="nodata"></i>sin datos</li>`);
  el.innerHTML = `<ul>${rows.join('')}</ul>`;
}

// With fewer classes than colors, spread them over the ramp so the darkest still means "most".
const colorSlot = (i, n) => (n <= 1 ? 4 : Math.round((i * 4) / (n - 1)));
const fillFor = (value, breaks) => {
  const c = classOf(value, breaks);
  return c < 0 ? css('--nodata') : seqColors()[colorSlot(c, breaks.length)];
};

function status(id, text) {
  $(id).textContent = text || '';
}

function fillSelects() {
  for (const sel of document.querySelectorAll('select.nat')) {
    sel.innerHTML = meta.nationalities.map((n, i) => `<option value="${i}">${n}</option>`).join('');
    sel.value = '2'; // Total Extranjeros
  }
  for (const sel of document.querySelectorAll('select.sex')) {
    sel.innerHTML = SEXES.map(([v, l]) => `<option value="${v}">${l}</option>`).join('');
  }
  const provinceOptions = meta.provinces.map((p) => `<option value="${p.code}">${p.name}</option>`).join('');
  for (const id of ['s-province', 'm-province', 'h-province']) {
    $(id).innerHTML = provinceOptions;
    $(id).value = DEFAULT_PROVINCE;
  }
}

const municipalitiesOf = (province) =>
  Object.entries(meta.municipalities)
    .filter(([code]) => code.startsWith(province))
    .map(([code, name]) => ({ code, name }))
    .sort((a, b) => a.name.localeCompare(b.name, 'es'));

const yearIndex = () => meta.years.indexOf(state.year);

// ---------- view: sections ----------
const sections = { map: null, layer: null, picker: null, token: 0, boundsKey: '', selected: new Map(), shown: [], pop: null, index: null };

function sectionsControls() {
  return { province: $('s-province').value, nat: +$('s-nat').value, sex: $('s-sex').value, percent: $('s-percent').checked };
}

async function renderSections({ focus } = {}) {
  if (!sections.map) {
    sections.map = makeMap($('s-map'));
    const LocateControl = L.Control.extend({
      onAdd() {
        const b = L.DomUtil.create('button', 'leaflet-bar locate-button');
        Object.assign(b, { type: 'button', title: 'Mi ubicación', innerHTML: '&#x2316;' });
        b.setAttribute('aria-label', 'Mi ubicación');
        L.DomEvent.on(b, 'click', (e) => { L.DomEvent.stop(e); locate(); });
        return b;
      },
    });
    new LocateControl({ position: 'topleft' }).addTo(sections.map);
  }
  const token = ++sections.token;
  const c = sectionsControls();
  status('s-status', 'Cargando…');
  let fc;
  let pop;
  try {
    [fc, pop] = await Promise.all([loadSections(state.year, c.province), loadPopulation(state.year, c.province)]);
  } catch {
    if (token === sections.token) status('s-status', 'No se pudieron cargar los datos. Comprueba la conexión.');
    return;
  }
  if (token !== sections.token) return;
  const index = indexSections(pop);
  const chosen = new Set(sections.picker.selected);
  const features = chosen.size ? fc.features.filter((f) => chosen.has(f.properties.m)) : fc.features;
  const valueOf = (f) => {
    const row = index.get(f.properties.id);
    return c.percent ? rowPercent(pop, row, c.nat, c.sex) : rowValue(pop, row, c.nat, c.sex);
  };
  const values = features.map(valueOf);
  const breaks = quantileBreaks(values);
  Object.assign(sections, { pop, index, shown: features });

  if (sections.layer) sections.layer.remove();
  const outline = css('--outline');
  const selectedColor = css('--text-primary');
  sections.layer = L.geoJSON({ type: 'FeatureCollection', features }, {
    style: (f) => {
      const sel = sections.selected.has(f.properties.id);
      return { fillColor: fillFor(valueOf(f), breaks), fillOpacity: 0.7, color: sel ? selectedColor : outline, weight: sel ? 3 : 0.6, opacity: sel ? 1 : 0.8 };
    },
    onEachFeature: (f, layer) => {
      layer.bindTooltip(() => `${f.properties.n}<br><b>${fmt(valueOf(f), c.percent)}</b>`, { sticky: true });
      layer.bindPopup(() => sectionPopup(f, c));
    },
  }).addTo(sections.map);

  const key = `${state.year}|${c.province}|${[...chosen].sort()}`;
  if (features.length && (focus || key.split('|').slice(1).join('|') !== sections.boundsKey)) {
    sections.map.fitBounds(sections.layer.getBounds(), { padding: [12, 12] });
  }
  sections.boundsKey = key.split('|').slice(1).join('|');
  legend($('s-legend'), breaks, c.percent);
  const missing = chosen.size && !features.length;
  status('s-status', missing ? `Ese municipio no tiene secciones en ${state.year}.` : `${nf.format(features.length)} secciones · ${state.year}`);
  $('s-kml').disabled = sections.selected.size === 0;
  $('s-kml').textContent = sections.selected.size ? `Descargar KML (${sections.selected.size})` : 'Descargar KML';
}

function sectionRows(f, c) {
  const row = sections.index.get(f.properties.id);
  const pop = sections.pop;
  const nat = meta.nationalities[c.nat];
  const rows = [
    ['Municipio', f.properties.n],
    ['Sección censal', f.properties.id],
    ['Año', String(state.year)],
    ['Nacionalidad', nat],
    ['Total', fmt(rowValue(pop, row, c.nat, 'both'))],
    ['Hombres', fmt(rowValue(pop, row, c.nat, 'men'))],
    ['Mujeres', fmt(rowValue(pop, row, c.nat, 'women'))],
  ];
  if (c.nat !== TOTAL) rows.push(['% de la población', fmt(rowPercent(pop, row, c.nat, 'both'), true)]);
  return rows;
}

function sectionPopup(f, c) {
  const el = document.createElement('div');
  el.className = 'popup';
  const id = f.properties.id;
  el.innerHTML = `<table>${sectionRows(f, c).map(([k, v]) => `<tr><th>${k}</th><td>${v}</td></tr>`).join('')}</table>`;
  const b = document.createElement('button');
  b.type = 'button';
  b.textContent = sections.selected.has(id) ? 'Quitar de la descarga KML' : 'Añadir a la descarga KML';
  b.addEventListener('click', () => {
    if (sections.selected.has(id)) sections.selected.delete(id);
    else sections.selected.set(id, f);
    sections.map.closePopup();
    renderSections();
  });
  el.append(b);
  return el;
}

async function downloadKml() {
  const c = sectionsControls();
  const items = [...sections.selected.values()].map((f) => ({
    feature: f,
    title: `${f.properties.n} · ${f.properties.id}`,
    rows: sectionRows(f, c),
  }));
  await saveTextFile(`secciones-${state.year}.kml`, buildKml(`Mapa de Nacionalidades ${state.year}`, items), 'application/vnd.google-earth.kml+xml');
}

const csvCell = (v) => (typeof v === 'string' && /[",;\n]/.test(v) ? `"${v.replace(/"/g, '""')}"` : v ?? '');
const toCsv = (rows) => rows.map((r) => r.map(csvCell).join(',')).join('\n') + '\n';

async function downloadSectionsCsv() {
  const c = sectionsControls();
  const { pop, index } = sections;
  const nat = meta.nationalities[c.nat];
  const rows = [['seccion_censal', 'municipio_codigo', 'municipio', 'anio', 'nacionalidad', 'hombres', 'mujeres', 'total', 'total_poblacion']];
  for (const f of sections.shown) {
    const r = index.get(f.properties.id);
    rows.push([f.properties.id, f.properties.m, f.properties.n, state.year, nat,
      rowValue(pop, r, c.nat, 'men'), rowValue(pop, r, c.nat, 'women'), rowValue(pop, r, c.nat), rowValue(pop, r, TOTAL)]);
  }
  await saveTextFile(`secciones-${c.province}-${state.year}.csv`, toCsv(rows), 'text/csv');
}

async function locate() {
  status('s-status', 'Buscando tu ubicación…');
  let pos;
  try {
    pos = await currentPosition();
  } catch (e) {
    status('s-status', e.message === 'permission' ? 'Sin permiso de ubicación.' : 'No se pudo obtener la ubicación.');
    return;
  }
  const provinces = await loadProvinces();
  const p = findFeature(provinces.features, pos.lng, pos.lat);
  if (!p) {
    status('s-status', 'Tu ubicación está fuera de España.');
    return;
  }
  $('s-province').value = p.properties.p;
  const fc = await loadSections(state.year, p.properties.p);
  const s = findFeature(fc.features, pos.lng, pos.lat);
  sections.picker.setItems(municipalitiesOf(p.properties.p), s ? [s.properties.m] : []);
  await renderSections({ focus: true });
  sections.here?.remove();
  sections.here = L.circleMarker([pos.lat, pos.lng], { radius: 7, color: '#fff', weight: 2, fillColor: css('--series-2'), fillOpacity: 1 })
    .bindTooltip('Estás aquí')
    .addTo(sections.map);
  if (s) {
    sections.map.setView([pos.lat, pos.lng], 15);
    sections.layer.eachLayer((l) => l.feature.properties.id === s.properties.id && l.openPopup([pos.lat, pos.lng]));
  }
}

// ---------- view: municipalities ----------
const munis = { chart: null, token: 0, rows: [] };

async function renderMunicipalities() {
  const token = ++munis.token;
  const province = $('m-province').value;
  const nat = +$('m-nat').value;
  const sex = $('m-sex').value;
  const percent = $('m-percent').checked;
  status('m-status', 'Cargando…');
  let pop;
  try {
    pop = await loadPopulation(state.year, province);
  } catch {
    if (token === munis.token) status('m-status', 'No se pudieron cargar los datos.');
    return;
  }
  if (token !== munis.token) return;
  const agg = byMunicipality(pop, nat);
  let rows = [...agg.entries()].map(([code, a]) => ({ code, name: meta.municipalities[code] ?? code, a, v: measure(a, sex, percent) }));
  rows.sort($('m-sort').checked ? (x, y) => (y.v ?? -1) - (x.v ?? -1) : (x, y) => x.name.localeCompare(y.name, 'es'));
  munis.rows = rows;

  const colors = seriesColors();
  const stacked = sex === 'both' && !percent;
  const datasets = stacked
    ? [
        { label: 'Hombres', data: rows.map((r) => r.a.m), backgroundColor: colors[0] },
        { label: 'Mujeres', data: rows.map((r) => r.a.w), backgroundColor: colors[1] },
      ]
    : [{ label: percent ? '% de la población' : 'Personas', data: rows.map((r) => r.v), backgroundColor: colors[0] }];
  $('m-wrap').style.height = `${Math.max(320, rows.length * 22 + 60)}px`;
  if (munis.chart) munis.chart.destroy();
  munis.chart = new Chart($('m-chart'), {
    type: 'bar',
    data: { labels: rows.map((r) => r.name), datasets },
    options: chartOptions({
      indexAxis: 'y',
      stacked,
      percent,
      legend: stacked,
      tooltip: (ctx) => `${ctx.dataset.label}: ${fmt(ctx.parsed.x, percent)}`,
    }),
  });
  const name = meta.provinces.find((p) => p.code === province)?.name;
  status('m-status', `${name} · ${nf.format(rows.length)} municipios · ${state.year}`);
}

function chartOptions({ indexAxis = 'x', stacked = false, percent, legend: showLegend, tooltip }) {
  const text = css('--text-secondary');
  const grid = css('--grid');
  const valueAxis = indexAxis === 'y' ? 'x' : 'y';
  const scales = {
    [valueAxis]: {
      stacked,
      beginAtZero: true,
      grid: { color: grid },
      ticks: { color: text, callback: (v) => (percent ? `${pf.format(v)} %` : nf.format(v)) },
    },
    [indexAxis === 'y' ? 'y' : 'x']: { stacked, grid: { display: false }, ticks: { color: text, autoSkip: indexAxis !== 'y' } },
  };
  return {
    indexAxis,
    maintainAspectRatio: false,
    animation: false,
    interaction: { mode: 'index', intersect: false },
    scales,
    elements: { bar: { borderRadius: 4, borderSkipped: 'start' }, line: { borderWidth: 2 }, point: { radius: 4, hoverRadius: 6 } },
    datasets: { bar: { barPercentage: 0.8, categoryPercentage: 0.85 } },
    plugins: {
      legend: { display: showLegend, labels: { color: text, usePointStyle: true } },
      tooltip: { callbacks: { label: tooltip } },
    },
  };
}

async function downloadMunicipalitiesCsv() {
  const nat = meta.nationalities[+$('m-nat').value];
  const rows = [['municipio_codigo', 'municipio', 'anio', 'nacionalidad', 'hombres', 'mujeres', 'total', 'total_poblacion']];
  for (const r of munis.rows) rows.push([r.code, r.name, state.year, nat, r.a.m, r.a.w, r.a.m + r.a.w, r.a.totalM + r.a.totalW]);
  await saveTextFile(`municipios-${$('m-province').value}-${state.year}.csv`, toCsv(rows), 'text/csv');
}

// ---------- view: Spain ----------
const spain = { map: null, layer: null, fitted: false };

async function renderSpain() {
  if (!spain.map) spain.map = makeMap($('e-map'));
  const provinces = await loadProvinces();
  const nat = +$('e-nat').value;
  const sex = $('e-sex').value;
  const percent = $('e-percent').checked;
  const year = trends.provinces[yearIndex()];
  const valueOf = (f) => measure(provinceAggregate(year, f.properties.p, nat), sex, percent);
  const breaks = quantileBreaks(provinces.features.map(valueOf));
  const names = new Map(meta.provinces.map((p) => [p.code, p.name]));
  if (spain.layer) spain.layer.remove();
  spain.layer = L.geoJSON(provinces, {
    style: (f) => ({ fillColor: fillFor(valueOf(f), breaks), fillOpacity: 0.75, color: css('--outline'), weight: 1 }),
    onEachFeature: (f, layer) => {
      layer.bindTooltip(() => `${names.get(f.properties.p)}<br><b>${fmt(valueOf(f), percent)}</b>`, { sticky: true });
      layer.on('click', () => {
        $('s-province').value = f.properties.p;
        $('s-nat').value = String(nat);
        $('s-sex').value = sex;
        $('s-percent').checked = percent;
        sections.picker.setItems(municipalitiesOf(f.properties.p));
        show('sections');
      });
    },
  }).addTo(spain.map);
  if (!spain.fitted) {
    spain.map.fitBounds([[35.8, -9.6], [43.9, 4.4]]);
    spain.fitted = true;
  }
  legend($('e-legend'), breaks, percent);
  const total = measure(nationalAggregate(year, nat), sex, percent);
  $('e-total').innerHTML = `España, ${state.year}<br><b>${fmt(total, percent)}</b><br><small>${meta.nationalities[nat]}${percent ? '' : ' · personas'}</small>`;
}

// ---------- view: history ----------
const hist = { chart: null, token: 0, provincePicker: null, muniPicker: null, colorOf: new Map() };

function colorFor(key, active) {
  // Colour follows the entity: a series keeps its colour while it stays selected.
  for (const k of [...hist.colorOf.keys()]) if (!active.includes(k)) hist.colorOf.delete(k);
  if (!hist.colorOf.has(key)) {
    const used = new Set(hist.colorOf.values());
    hist.colorOf.set(key, [0, 1, 2, 3, 4, 5, 6, 7].find((i) => !used.has(i)) ?? 0);
  }
  return seriesColors()[hist.colorOf.get(key)];
}

async function renderHistory() {
  const token = ++hist.token;
  const level = document.querySelector('input[name="h-level"]:checked').value;
  const nat = +$('h-nat').value;
  const sex = $('h-sex').value;
  const percent = $('h-percent').checked;
  $('h-provinces-field').hidden = level !== 'province';
  $('h-province-field').hidden = level !== 'municipality';
  $('h-municipalities-field').hidden = level !== 'municipality';

  let series = [];
  if (level === 'national') {
    series = [{ key: 'ES', name: 'España', values: trends.provinces.map((y) => measure(nationalAggregate(y, nat), sex, percent)) }];
  } else if (level === 'province') {
    const names = new Map(meta.provinces.map((p) => [p.code, p.name]));
    series = hist.provincePicker.selected.map((code) => ({
      key: code,
      name: names.get(code),
      values: trends.provinces.map((y) => measure(provinceAggregate(y, code, nat), sex, percent)),
    }));
  } else {
    const province = $('h-province').value;
    const chosen = hist.muniPicker.selected;
    if (chosen.length) {
      status('h-status', 'Cargando…');
      let pops;
      try {
        pops = await Promise.all(meta.years.map((y) => loadPopulation(y, province)));
      } catch {
        if (token === hist.token) status('h-status', 'No se pudieron cargar los datos.');
        return;
      }
      if (token !== hist.token) return;
      const perYear = pops.map((p) => byMunicipality(p, nat));
      series = chosen.map((code) => ({ key: code, name: meta.municipalities[code], values: perYear.map((m) => measure(m.get(code), sex, percent)) }));
    }
  }
  if (token !== hist.token) return;
  const active = series.map((s) => s.key);
  if (hist.chart) hist.chart.destroy();
  hist.chart = new Chart($('h-chart'), {
    type: 'line',
    data: {
      labels: meta.years.map(String),
      datasets: series.map((s) => {
        const color = colorFor(s.key, active);
        return { label: s.name, data: s.values, borderColor: color, backgroundColor: color, spanGaps: false };
      }),
    },
    options: chartOptions({ percent, legend: series.length > 1, tooltip: (ctx) => `${ctx.dataset.label}: ${fmt(ctx.parsed.y, percent)}` }),
  });
  const nationalityNote = meta.nationalities[nat].includes('Europa') || nat === 3
    ? ' · 2021: el Reino Unido pasa de la UE a «Resto de Europa»'
    : '';
  status('h-status', series.length ? `${meta.nationalities[nat]}${nationalityNote}` : 'Elige al menos uno.');
}

// ---------- navigation ----------
const renderers = { sections: renderSections, municipalities: renderMunicipalities, spain: renderSpain, history: renderHistory, about: () => {} };

function show(view) {
  state.view = view;
  for (const b of document.querySelectorAll('.tabs button')) b.toggleAttribute('aria-current', b.dataset.view === view);
  for (const v of document.querySelectorAll('.view')) v.hidden = v.id !== `view-${view}`;
  window.history.replaceState(null, '', `#${view}`);
  renderers[view]();
  requestAnimationFrame(() => {
    sections.map?.invalidateSize();
    spain.map?.invalidateSize();
  });
}

async function init() {
  [meta, trends] = await Promise.all([loadMeta(), loadTrends()]);
  $('version').textContent = pkg.version;
  const yr = $('year');
  yr.min = String(meta.years[0]);
  yr.max = String(meta.years.at(-1));
  yr.value = String(state.year);
  fillSelects();

  sections.picker = new Picker($('s-municipalities'), { placeholder: 'Buscar municipio…', onChange: () => renderSections() });
  sections.picker.setItems(municipalitiesOf(DEFAULT_PROVINCE), ['30030']);
  hist.provincePicker = new Picker($('h-provinces'), { max: 8, placeholder: 'Buscar provincia…', onChange: () => renderHistory() });
  hist.provincePicker.setItems(meta.provinces.map((p) => ({ code: p.code, name: p.name })), ['28', '08', DEFAULT_PROVINCE]);
  hist.muniPicker = new Picker($('h-municipalities'), { max: 8, placeholder: 'Buscar municipio…', onChange: () => renderHistory() });
  hist.muniPicker.setItems(municipalitiesOf(DEFAULT_PROVINCE), ['30030']);

  yr.addEventListener('input', () => {
    $('yearOut').textContent = yr.value;
  });
  yr.addEventListener('change', () => {
    state.year = +yr.value;
    renderers[state.view]();
  });
  $('s-province').addEventListener('change', () => {
    sections.picker.setItems(municipalitiesOf($('s-province').value));
    sections.selected.clear();
    renderSections({ focus: true });
  });
  for (const id of ['s-nat', 's-sex', 's-percent']) $(id).addEventListener('change', () => renderSections());
  $('s-kml').addEventListener('click', downloadKml);
  $('s-csv').addEventListener('click', downloadSectionsCsv);
  $('s-locate').addEventListener('click', locate);
  for (const id of ['m-province', 'm-nat', 'm-sex', 'm-percent', 'm-sort']) $(id).addEventListener('change', renderMunicipalities);
  $('m-csv').addEventListener('click', downloadMunicipalitiesCsv);
  for (const id of ['e-nat', 'e-sex', 'e-percent']) $(id).addEventListener('change', renderSpain);
  for (const id of ['h-nat', 'h-sex', 'h-percent']) $(id).addEventListener('change', renderHistory);
  for (const r of document.querySelectorAll('input[name="h-level"]')) r.addEventListener('change', renderHistory);
  $('h-province').addEventListener('change', () => {
    hist.muniPicker.setItems(municipalitiesOf($('h-province').value));
    renderHistory();
  });
  for (const b of document.querySelectorAll('.tabs button')) b.addEventListener('click', () => show(b.dataset.view));

  dark.addEventListener('change', () => renderers[state.view]());

  // On phones the filters fold away so the map gets the screen.
  for (const aside of document.querySelectorAll('.controls')) {
    const b = Object.assign(document.createElement('button'), { type: 'button', className: 'toggle-controls', textContent: 'Ocultar opciones' });
    b.addEventListener('click', () => {
      const collapsed = aside.classList.toggle('collapsed');
      b.textContent = collapsed ? 'Mostrar opciones' : 'Ocultar opciones';
      requestAnimationFrame(() => [sections.map, spain.map].forEach((m) => m?.invalidateSize()));
    });
    aside.prepend(b);
  }

  const fromHash = location.hash.slice(1);
  show(renderers[fromHash] ? fromHash : 'sections');
}

init().catch((e) => {
  document.querySelector('main').innerHTML = `<p class="status">No se pudo iniciar la aplicación: ${e.message}</p>`;
});
