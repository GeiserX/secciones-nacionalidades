// Pure functions over the data files. No DOM, no fetch: everything here is unit-tested.
//
// A population file (pop/<year>/<province>.json) holds, for one province and year:
//   s: section ids (10 digits: province 2 + municipality 3 + district 2 + section 3)
//   m: men per nationality, one row per section
//   w: women per nationality, one row per section
// Nationality index 0 is "Total Población".

export const TOTAL = 0;

/** Index from section id to its row in a population file. */
export function indexSections(pop) {
  const index = new Map();
  pop.s.forEach((id, i) => index.set(id, i));
  return index;
}

/** People of one nationality in one row, for sex 'both' | 'men' | 'women'. */
export function rowValue(pop, row, nat, sex = 'both') {
  if (row === undefined || row < 0) return null;
  const m = pop.m[row][nat];
  const w = pop.w[row][nat];
  if (sex === 'men') return m;
  if (sex === 'women') return w;
  return m + w;
}

/** Share of the row's total population, in percent, or null when the total is 0. */
export function rowPercent(pop, row, nat, sex = 'both') {
  const total = rowValue(pop, row, TOTAL, sex);
  if (!total) return null;
  return (100 * rowValue(pop, row, nat, sex)) / total;
}

/** Municipality code (5 digits) of a section id. */
export const municipalityOf = (sectionId) => sectionId.slice(0, 5);

/**
 * Sums a population file by municipality.
 * Returns Map code -> { m: number, w: number, totalM: number, totalW: number } for one nationality.
 */
export function byMunicipality(pop, nat) {
  const out = new Map();
  pop.s.forEach((id, i) => {
    const code = municipalityOf(id);
    let acc = out.get(code);
    if (!acc) out.set(code, (acc = { m: 0, w: 0, totalM: 0, totalW: 0 }));
    acc.m += pop.m[i][nat];
    acc.w += pop.w[i][nat];
    acc.totalM += pop.m[i][TOTAL];
    acc.totalW += pop.w[i][TOTAL];
  });
  return out;
}

/** Picks the number to show from an aggregate { m, w, totalM, totalW }. */
export function measure(agg, sex, percent) {
  if (!agg) return null;
  const v = sex === 'men' ? agg.m : sex === 'women' ? agg.w : agg.m + agg.w;
  if (!percent) return v;
  const t = sex === 'men' ? agg.totalM : sex === 'women' ? agg.totalW : agg.totalM + agg.totalW;
  return t ? (100 * v) / t : null;
}

/** Aggregate for a whole province from trends.json: { m, w, totalM, totalW }. */
export function provinceAggregate(trendsYear, province, nat) {
  const p = trendsYear[province];
  if (!p) return null;
  return { m: p.m[nat], w: p.w[nat], totalM: p.m[TOTAL], totalW: p.w[TOTAL] };
}

/** Aggregate for Spain from trends.json for one year. */
export function nationalAggregate(trendsYear, nat) {
  const acc = { m: 0, w: 0, totalM: 0, totalW: 0 };
  for (const p of Object.values(trendsYear)) {
    acc.m += p.m[nat];
    acc.w += p.w[nat];
    acc.totalM += p.m[TOTAL];
    acc.totalW += p.w[TOTAL];
  }
  return acc;
}

/**
 * Class breaks for a choropleth: up to `classes` quantile classes over the finite values.
 * Returns ascending upper bounds (the last one is the maximum). Equal values give one class.
 */
export function quantileBreaks(values, classes = 5) {
  const v = values.filter((x) => x !== null && Number.isFinite(x)).sort((a, b) => a - b);
  if (v.length === 0) return [];
  const breaks = [];
  for (let k = 1; k <= classes; k++) {
    const q = v[Math.min(v.length - 1, Math.ceil((k / classes) * v.length) - 1)];
    if (breaks.length === 0 || q > breaks[breaks.length - 1]) breaks.push(q);
  }
  return breaks;
}

/** Class index of a value given the breaks from quantileBreaks, or -1 for no data. */
export function classOf(value, breaks) {
  if (value === null || !Number.isFinite(value) || breaks.length === 0) return -1;
  for (let i = 0; i < breaks.length; i++) if (value <= breaks[i]) return i;
  return breaks.length - 1;
}
