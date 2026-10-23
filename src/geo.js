// Point-in-polygon for GeoJSON features (lng/lat), used to find the user's section.

function inRing(lng, lat, ring) {
  let inside = false;
  for (let i = 0, j = ring.length - 1; i < ring.length; j = i++) {
    const [xi, yi] = ring[i];
    const [xj, yj] = ring[j];
    if (yi > lat !== yj > lat && lng < ((xj - xi) * (lat - yi)) / (yj - yi) + xi) inside = !inside;
  }
  return inside;
}

const inPolygon = (lng, lat, rings) => inRing(lng, lat, rings[0]) && !rings.slice(1).some((h) => inRing(lng, lat, h));

export function containsPoint(feature, lng, lat) {
  const g = feature.geometry;
  if (!g) return false;
  if (g.type === 'Polygon') return inPolygon(lng, lat, g.coordinates);
  if (g.type === 'MultiPolygon') return g.coordinates.some((p) => inPolygon(lng, lat, p));
  return false;
}

export const findFeature = (features, lng, lat) => features.find((f) => containsPoint(f, lng, lat));
