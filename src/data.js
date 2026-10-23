// Loads the static data files (public/data) once and keeps them in memory.
import { feature } from 'topojson-client';

const base = new URL('data/', document.baseURI);
const cache = new Map();

function load(path) {
  if (!cache.has(path)) {
    const p = fetch(new URL(path, base)).then((r) => {
      if (!r.ok) throw new Error(`${path}: HTTP ${r.status}`);
      return r.json();
    });
    p.catch(() => cache.delete(path));
    cache.set(path, p);
  }
  return cache.get(path);
}

const toGeoJson = (topo) => feature(topo, Object.values(topo.objects)[0]);

export const loadMeta = () => load('meta.json');
export const loadTrends = () => load('trends.json');
export const loadPopulation = (year, province) => load(`pop/${year}/${province}.json`);
export const loadSections = (year, province) => load(`geo/${year}/${province}.json`).then(toGeoJson);
export const loadProvinces = () => load('geo/provinces.json').then(toGeoJson);
