# Desarrollo

## Estructura

| Ruta | Qué es |
|---|---|
| `index.html`, `src/` | La aplicación web (Vite, Leaflet, Chart.js). Todo el texto está en español |
| `public/data/` | Los datos que carga la aplicación, generados por los scripts de abajo y versionados en git |
| `poblacion/`, `seccionado/` | Los datos originales del INE (2012-2021) en RDS y PC-Axis |
| `scripts/` | La conversión de los datos originales a `public/data/` |
| `android/`, `ios/` | Las aplicaciones nativas (Capacitor), con los datos dentro para funcionar sin conexión |
| `tests/web/` | Pruebas (Vitest) |

## Formato de `public/data`

- `meta.json`: años, nacionalidades (índice 0 = «Total Población»), provincias y nombres de municipios.
- `trends.json`: hombres y mujeres por año, provincia y nacionalidad.
- `pop/<año>/<provincia>.json`: `{ s: [secciones], m: [[hombres por nacionalidad]], w: [[mujeres …]] }`. El total es la suma de hombres y mujeres; la exportación comprueba que coincide con el «Ambos sexos» del INE en cada sección.
- `geo/<año>/<provincia>.json`: contornos de las secciones en TopoJSON (WGS84, simplificados), con `id` (sección), `m` (municipio), `p` (provincia) y `n` (nombre del municipio).
- `geo/provinces.json`: contornos de las provincias, para el mapa de España.

## Regenerar los datos

Solo hace falta si cambian los datos originales o el formato. Es pesado (unos 20 minutos y varios GB de memoria): hazlo en un servidor, no en un portátil.

```bash
docker run --rm -v "$PWD":/w -w /w rocker/geospatial:4.5.1 Rscript scripts/export-data.R
docker run --rm -v "$PWD":/w -w /w node:22-alpine sh scripts/build-data.sh
```

## Aplicaciones móviles

```bash
npm run cap:sync          # build web + copia a android/ e ios/
npx cap open android      # Android Studio
npx cap open ios          # Xcode
```

## Logo, iconos y banners

`scripts/build-brand.mjs` dibuja en SVG, a partir de los datos, el logo (`public/icon.svg`), los iconos y pantallas de inicio de las apps (`assets/*.svg`) y los banners (`docs/images/banner.svg` para el README y `feature-graphic.svg` para Google Play). El logo es España con cada provincia coloreada según su porcentaje de extranjeros en 2021. Después se pasan a PNG sin transparencia (salvo `icon-foreground`) y se generan los tamaños de cada plataforma:

```bash
node scripts/build-brand.mjs
for f in assets/*.svg docs/images/feature-graphic.svg; do npx --yes sharp-cli@5 -i "$f" -o "${f%.svg}.png"; done
for f in assets/icon-only assets/icon-background assets/splash assets/splash-dark docs/images/feature-graphic; do
  npx --yes sharp-cli@5 -i "$f.png" -o "$f.png" flatten --background "#1a1a19"
done
npx sharp-cli@5 -i docs/images/banner.svg -o docs/images/social-preview.png
npx --yes @capacitor/assets@3.0.5 generate --iconBackgroundColor '#1a1a19' --iconBackgroundColorDark '#1a1a19' --splashBackgroundColor '#1a1a19' --splashBackgroundColorDark '#1a1a19' --android --ios
```
