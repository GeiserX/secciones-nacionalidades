// Builds the small summary files the app needs before any province is opened:
//   meta.json        years, nationalities, provinces, municipalities (code -> name)
//   trends.json      men and women per year, province and nationality
// Usage: node scripts/build-derived.mjs build/raw public/data
import { readFileSync, writeFileSync, readdirSync, statSync } from 'node:fs';
import { join } from 'node:path';

const [raw, out] = process.argv.slice(2);
const meta = JSON.parse(readFileSync(join(raw, 'meta.json'), 'utf8'));
const years = meta.years.map(Number);

// Municipality names: the newest name wins, municipalities that disappeared keep their last one.
const municipalities = {};
const sectionsPerYear = {};
for (const y of years) {
  const dir = join(out, 'geo', String(y));
  let n = 0;
  for (const file of readdirSync(dir)) {
    // One file per province, named by its code. Anything else means mapshaper split a layer.
    if (!/^\d{2}\.json$/.test(file)) throw new Error(`unexpected file geo/${y}/${file}`);
    const topo = JSON.parse(readFileSync(join(dir, file), 'utf8'));
    for (const obj of Object.values(topo.objects)) {
      for (const g of obj.geometries) {
        if (g.type !== 'Polygon' && g.type !== 'MultiPolygon') throw new Error(`${y}/${file}: ${g.type}`);
        municipalities[g.properties.m] = g.properties.n;
        n++;
      }
    }
  }
  sectionsPerYear[y] = n;
}

// trends[yearIndex][province] = { m: [per nationality], w: [...] }
const trends = years.map((y) => {
  const dir = join(out, 'pop', String(y));
  const byProvince = {};
  for (const file of readdirSync(dir)) {
    const p = file.replace('.json', '');
    const d = JSON.parse(readFileSync(join(dir, file), 'utf8'));
    const sum = (rows) => rows.reduce((acc, r) => acc.map((v, i) => v + r[i]), new Array(meta.nationalities.length).fill(0));
    byProvince[p] = { m: sum(d.m), w: sum(d.w) };
  }
  return byProvince;
});

// Province names come from INE's code list, not from the boundary files: the 2012 file
// misspells Gipuzkoa ("Gipuzcoa") and labels some sections of Palencia as Burgos.
const PROVINCES = {
  '01': 'Araba/Álava', '02': 'Albacete', '03': 'Alicante/Alacant', '04': 'Almería', '05': 'Ávila', '06': 'Badajoz',
  '07': 'Balears, Illes', '08': 'Barcelona', '09': 'Burgos', '10': 'Cáceres', '11': 'Cádiz', '12': 'Castellón/Castelló',
  '13': 'Ciudad Real', '14': 'Córdoba', '15': 'Coruña, A', '16': 'Cuenca', '17': 'Girona', '18': 'Granada',
  '19': 'Guadalajara', '20': 'Gipuzkoa', '21': 'Huelva', '22': 'Huesca', '23': 'Jaén', '24': 'León', '25': 'Lleida',
  '26': 'Rioja, La', '27': 'Lugo', '28': 'Madrid', '29': 'Málaga', '30': 'Murcia', '31': 'Navarra', '32': 'Ourense',
  '33': 'Asturias', '34': 'Palencia', '35': 'Palmas, Las', '36': 'Pontevedra', '37': 'Salamanca',
  '38': 'Santa Cruz de Tenerife', '39': 'Cantabria', '40': 'Segovia', '41': 'Sevilla', '42': 'Soria', '43': 'Tarragona',
  '44': 'Teruel', '45': 'Toledo', '46': 'Valencia/València', '47': 'Valladolid', '48': 'Bizkaia', '49': 'Zamora',
  '50': 'Zaragoza', '51': 'Ceuta', '52': 'Melilla',
};
const codes = new Set(meta.provinces.map((p) => p.code));
if (codes.size !== 52 || [...codes].some((c) => !PROVINCES[c])) throw new Error(`unexpected province codes: ${[...codes]}`);
const provinces = Object.entries(PROVINCES)
  .map(([code, name]) => ({ code, name }))
  .sort((a, b) => a.name.localeCompare(b.name, 'es'));
writeFileSync(join(out, 'meta.json'), JSON.stringify({
  years, nationalities: meta.nationalities, provinces, municipalities, sectionsPerYear,
}));
writeFileSync(join(out, 'trends.json'), JSON.stringify({ years, provinces: trends }));

let total = 0;
const walk = (d) => readdirSync(d).forEach((f) => { const p = join(d, f); const s = statSync(p); s.isDirectory() ? walk(p) : (total += s.size); });
walk(out);
console.log(`public/data: ${(total / 1e6).toFixed(1)} MB, sections per year:`, sectionsPerYear);
