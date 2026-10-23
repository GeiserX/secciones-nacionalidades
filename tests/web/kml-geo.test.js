import { describe, it, expect } from 'vitest';
import { buildKml } from '../../src/kml.js';
import { containsPoint, findFeature } from '../../src/geo.js';

const square = {
  type: 'Feature',
  properties: { id: '3003001001' },
  geometry: { type: 'Polygon', coordinates: [[[0, 0], [10, 0], [10, 10], [0, 10], [0, 0]], [[4, 4], [6, 4], [6, 6], [4, 6], [4, 4]]] },
};

describe('kml', () => {
  it('writes one placemark per feature with escaped text', () => {
    const kml = buildKml('Prueba & co', [{ feature: square, title: 'Murcia <1>', rows: [['Población', '1.234']] }]);
    expect(kml).toContain('<name>Prueba &amp; co</name>');
    expect(kml).toContain('<name>Murcia &lt;1&gt;</name>');
    expect(kml).toContain('<outerBoundaryIs><LinearRing><coordinates>0,0 10,0');
    expect(kml).toContain('<innerBoundaryIs>');
    expect(kml.match(/<Placemark>/g)).toHaveLength(1);
  });
});

describe('point in polygon', () => {
  it('finds points inside, outside and in holes', () => {
    expect(containsPoint(square, 2, 2)).toBe(true);
    expect(containsPoint(square, 5, 5)).toBe(false);
    expect(containsPoint(square, 11, 2)).toBe(false);
    expect(findFeature([square], 1, 9)).toBe(square);
  });
});
