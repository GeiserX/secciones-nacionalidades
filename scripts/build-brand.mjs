// Draws the logo, the app icons, the splash screens and the banners as SVG, from the app's own data:
// every province coloured by its share of foreign residents in 2021, and Madrid's census sections.
// Run: node scripts/build-brand.mjs, then the PNG step in docs/development.md.
// docs/images/banner.svg doubles as the GitHub social preview (social-preview.png).
import fs from 'node:fs';
import { feature, merge } from 'topojson-client';

const NAME = 'Mapa de Nacionalidades';
const TAG = 'España · extranjeros en cada sección censal, 2012–2021';
const BG = '#1a1a19';
const SEQ = ['#cde2fb', '#86b6ef', '#3987e5', '#1c5cab', '#0d366b'];
const YEAR = 2021;
const FOREIGN = 2; // "Total Extranjeros" in meta.json nationalities

const read = (p) => JSON.parse(fs.readFileSync(new URL(`../public/data/${p}`, import.meta.url), 'utf8'));
const features = (topo) => feature(topo, Object.values(topo.objects)[0]).features;
const K = Math.cos((40 * Math.PI) / 180);
// Outer rings only: an enclave's hole would show as a dark speck at icon size.
const rings = (g) => (g.type === 'Polygon' ? [g.coordinates] : g.coordinates).map((p) => p[0]);
// Islets smaller than this (in square degrees) are invisible at icon size and draw as specks.
const area = (r) => Math.abs(r.reduce((a, [x, y], i) => { const [x2, y2] = r[(i + 1) % r.length]; return a + x * y2 - x2 * y; }, 0)) / 2;
const visible = (g, min) => rings(g).filter((r) => area(r) >= min);

/** Fits features into box [x, y, w, h] with an equirectangular projection; returns SVG paths. */
function project(list, [bx, by, bw, bh], min = 0) {
  let [x0, y0, x1, y1] = [Infinity, Infinity, -Infinity, -Infinity];
  for (const f of list) for (const r of rings(f.geometry)) for (const [x, y] of r) {
    x0 = Math.min(x0, x * K); x1 = Math.max(x1, x * K); y0 = Math.min(y0, -y); y1 = Math.max(y1, -y);
  }
  const s = Math.min(bw / (x1 - x0), bh / (y1 - y0));
  const ox = bx + (bw - s * (x1 - x0)) / 2;
  const oy = by + (bh - s * (y1 - y0)) / 2;
  const pt = ([x, y]) => `${(ox + (x * K - x0) * s).toFixed(1)},${(oy + (-y - y0) * s).toFixed(1)}`;
  return list.map((f) => ({ f, d: visible(f.geometry, min).map((r) => `M${r.map(pt).join('L')}Z`).join('') }));
}

// Share of foreign residents per province, split into five equal-count classes.
const provincesTopo = read('geo/provinces.json');
const provinces = features(provincesTopo);
const share = new Map();
for (const f of provinces) {
  const pop = read(`pop/${YEAR}/${f.properties.p}.json`);
  let all = 0, foreign = 0;
  pop.m.forEach((row, i) => { all += row[0] + pop.w[i][0]; foreign += row[FOREIGN] + pop.w[i][FOREIGN]; });
  share.set(f.properties.p, foreign / all);
}
const sorted = [...share.values()].sort((a, b) => a - b);
const breaks = [1, 2, 3, 4].map((q) => sorted[Math.floor((q * sorted.length) / 5)]);
const shade = (v) => SEQ[breaks.filter((b) => v >= b).length];

/** Spain in box [x, y, size]: peninsula and Balearics, Canarias inset at the bottom left. */
function spain(x, y, size, stroke) {
  const main = provinces.filter((f) => !['35', '38', '51', '52'].includes(f.properties.p));
  const canarias = provinces.filter((f) => ['35', '38'].includes(f.properties.p));
  const mainBox = [x, y, size, size * 0.86];
  const paths = [...project(main, mainBox, 0.004), ...project(canarias, [x, y + size * 0.8, size * 0.3, size * 0.2], 0.004)];
  // Bardenas Reales and a few sierras belong to no municipality, so no section covers them;
  // a backing layer cut from the union of the provinces keeps them from showing as holes.
  const geoms = Object.values(provincesTopo.objects)[0].geometries.filter((g) => !['35', '38', '51', '52'].includes(g.properties.p));
  const [backing] = project([...main, { geometry: merge(provincesTopo, geoms) }], mainBox, 0.004).slice(-1);
  return `<path d="${backing.d}" fill="${SEQ[1]}"/>` + paths.map(({ f, d }) => `<path d="${d}" fill="${shade(share.get(f.properties.p))}" stroke="${BG}" stroke-width="${stroke}" stroke-linejoin="round"/>`).join('');
}

const svg = (w, h, body) => `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 ${w} ${h}" width="${w}" height="${h}">${body}</svg>\n`;
const out = (path, text) => fs.writeFileSync(new URL(`../${path}`, import.meta.url), text);

out('public/icon.svg', svg(512, 512, `<rect width="512" height="512" rx="112" fill="${BG}"/>${spain(48, 56, 416, 2.5)}`));
out('assets/icon-only.svg', svg(1024, 1024, `<rect width="1024" height="1024" fill="${BG}"/>${spain(112, 128, 800, 3)}`));
out('assets/icon-foreground.svg', svg(1024, 1024, spain(262, 272, 500, 2.5)));
out('assets/icon-background.svg', svg(1024, 1024, `<rect width="1024" height="1024" fill="${BG}"/>`));
for (const name of ['splash', 'splash-dark']) out(`assets/${name}.svg`, svg(2732, 2732, `<rect width="2732" height="2732" fill="${BG}"/>${spain(1066, 1080, 600, 3)}`));

// Banners: Madrid's sections by share of foreign residents, with the name on the left.
const madrid = read(`pop/${YEAR}/28.json`);
const row = new Map(madrid.s.map((id, i) => [id, i]));
const sectionShare = (id) => {
  const i = row.get(id);
  const all = i === undefined ? 0 : madrid.m[i][0] + madrid.w[i][0];
  return all ? (madrid.m[i][FOREIGN] + madrid.w[i][FOREIGN]) / all : 0;
};
const sections = features(read(`geo/${YEAR}/28.json`)).filter((f) => f.properties.m === '28079');
const cls = (v) => SEQ[[0.08, 0.12, 0.17, 0.25].filter((b) => v >= b).length];
function banner(w, h) {
  const map = project(sections, [w * 0.55, h * 0.05, w * 0.42, h * 0.9])
    .map(({ f, d }) => `<path d="${d}" fill="${cls(sectionShare(f.properties.id))}" stroke="#121211" stroke-width="0.6"/>`).join('');
  const font = `font-family="-apple-system, 'SF Pro Display', 'Helvetica Neue', Arial, sans-serif"`;
  return svg(w, h, `<rect width="${w}" height="${h}" fill="#121211"/>${map}<g ${font}>
<text x="${w * 0.06}" y="${h * 0.45}" font-size="${h * 0.085}" font-weight="700" fill="#fcfcfb">${NAME}</text>
<text x="${w * 0.06}" y="${h * 0.56}" font-size="${h * 0.042}" fill="#c9c8c2">${TAG}</text>
<text x="${w * 0.06}" y="${h * 0.88}" font-size="${h * 0.03}" fill="#8a8984">Madrid, ${YEAR} · % de población extranjera por sección censal · Datos: INE</text></g>`);
}
out('docs/images/banner.svg', banner(1280, 640));
out('docs/images/feature-graphic.svg', banner(1024, 500));
console.log('classes', breaks.map((b) => (100 * b).toFixed(1) + '%').join(' / '));
