import { it, expect } from 'vitest';
import { normalize } from '../../src/picker.js';

it('searches without accents or case', () => {
  expect(normalize('Alcalá de Henares').includes(normalize('alcala'))).toBe(true);
});
