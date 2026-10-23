// Builds a KML document (Google Earth, Google My Maps) from GeoJSON section features.

const escapeXml = (s) =>
  String(s).replace(/[<>&'"]/g, (c) => ({ '<': '&lt;', '>': '&gt;', '&': '&amp;', "'": '&apos;', '"': '&quot;' })[c]);

const ring = (coords) => coords.map(([lng, lat]) => `${lng},${lat}`).join(' ');

function polygonXml(rings) {
  const [outer, ...holes] = rings;
  return (
    '<Polygon><outerBoundaryIs><LinearRing><coordinates>' + ring(outer) + '</coordinates></LinearRing></outerBoundaryIs>' +
    holes.map((h) => '<innerBoundaryIs><LinearRing><coordinates>' + ring(h) + '</coordinates></LinearRing></innerBoundaryIs>').join('') +
    '</Polygon>'
  );
}

function geometryXml(geometry) {
  if (geometry.type === 'Polygon') return polygonXml(geometry.coordinates);
  if (geometry.type === 'MultiPolygon') return '<MultiGeometry>' + geometry.coordinates.map(polygonXml).join('') + '</MultiGeometry>';
  return '';
}

/**
 * @param {string} name document name
 * @param {Array<{feature: object, title: string, rows: Array<[string, string]>}>} items
 */
export function buildKml(name, items) {
  const placemarks = items
    .map(({ feature, title, rows }) => {
      const table = '<table>' + rows.map(([k, v]) => `<tr><td>${escapeXml(k)}</td><td><b>${escapeXml(v)}</b></td></tr>`).join('') + '</table>';
      return `<Placemark><name>${escapeXml(title)}</name><styleUrl>#s</styleUrl><description><![CDATA[${table}]]></description>${geometryXml(feature.geometry)}</Placemark>`;
    })
    .join('\n');
  return (
    '<?xml version="1.0" encoding="UTF-8"?>\n' +
    '<kml xmlns="http://www.opengis.net/kml/2.2"><Document>' +
    `<name>${escapeXml(name)}</name>` +
    '<Style id="s"><LineStyle><color>ffd6782a</color><width>2</width></LineStyle><PolyStyle><color>66d6782a</color></PolyStyle></Style>\n' +
    placemarks +
    '\n</Document></kml>\n'
  );
}
