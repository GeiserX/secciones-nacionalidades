import { describe, it, expect } from 'vitest';
import {
  indexSections, rowValue, rowPercent, byMunicipality, measure,
  nationalAggregate, provinceAggregate, quantileBreaks, classOf, municipalityOf,
} from '../../src/stats.js';

// Two sections of Murcia (30030) and one of Alcantarilla (30005); nationalities: total, Spanish, foreign.
const pop = {
  s: ['3003001001', '3003001002', '3000501001'],
  m: [[100, 80, 20], [50, 50, 0], [10, 5, 5]],
  w: [[120, 90, 30], [40, 40, 0], [0, 0, 0]],
};

describe('rows', () => {
  const idx = indexSections(pop);
  it('adds men and women for both sexes', () => {
    expect(rowValue(pop, idx.get('3003001001'), 2)).toBe(50);
    expect(rowValue(pop, idx.get('3003001001'), 2, 'men')).toBe(20);
    expect(rowValue(pop, idx.get('3003001001'), 2, 'women')).toBe(30);
  });
  it('returns null for a section without population data', () => {
    expect(rowValue(pop, idx.get('9999999999'), 2)).toBeNull();
  });
  it('computes the share of the section total', () => {
    expect(rowPercent(pop, 0, 2)).toBeCloseTo((100 * 50) / 220);
  });
  it('gives null instead of dividing by zero', () => {
    expect(rowPercent(pop, 2, 2, 'women')).toBeNull();
  });
});

describe('aggregation', () => {
  it('sums sections by municipality', () => {
    const m = byMunicipality(pop, 2);
    expect(municipalityOf('3003001002')).toBe('30030');
    expect(m.get('30030')).toEqual({ m: 20, w: 30, totalM: 150, totalW: 160 });
    expect(m.get('30005')).toEqual({ m: 5, w: 0, totalM: 10, totalW: 0 });
  });
  it('measures counts and percentages per sex', () => {
    const a = { m: 20, w: 30, totalM: 150, totalW: 160 };
    expect(measure(a, 'both', false)).toBe(50);
    expect(measure(a, 'women', true)).toBeCloseTo(18.75);
    expect(measure(null, 'both', false)).toBeNull();
  });
  it('sums provinces into Spain', () => {
    const year = { 30: { m: [10, 8, 2], w: [12, 9, 3] }, 28: { m: [20, 15, 5], w: [22, 16, 6] } };
    expect(nationalAggregate(year, 2)).toEqual({ m: 7, w: 9, totalM: 30, totalW: 34 });
    expect(provinceAggregate(year, '30', 2)).toEqual({ m: 2, w: 3, totalM: 10, totalW: 12 });
    expect(provinceAggregate(year, '01', 2)).toBeNull();
  });
});

describe('choropleth classes', () => {
  it('splits into quantiles and places every value in a class', () => {
    const values = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    const b = quantileBreaks(values, 5);
    expect(b).toEqual([2, 4, 6, 8, 10]);
    expect(values.map((v) => classOf(v, b))).toEqual([0, 0, 1, 1, 2, 2, 3, 3, 4, 4]);
  });
  it('collapses equal values into one class and ignores missing data', () => {
    expect(quantileBreaks([5, 5, null, 5])).toEqual([5]);
    expect(classOf(null, [5])).toBe(-1);
    expect(quantileBreaks([])).toEqual([]);
  });
});
