#!/usr/bin/env bash
# Turns build/raw (from export-data.R) into public/data, the files the app loads.
# Needs Node 22+. Heavy: run it on a server, not a laptop.
#
#   docker run --rm -v "$PWD":/w -w /w node:22-alpine sh scripts/build-data.sh
set -eu
RAW=build/raw
OUT=public/data
MAPSHAPER="npx -y mapshaper@0.7.97"

rm -rf "$OUT"
mkdir -p "$OUT/geo" "$OUT/pop"
cp -r "$RAW/pop/." "$OUT/pop/"

for f in "$RAW"/geo-*.geojson; do
  y=$(basename "$f" .geojson | cut -d- -f2)
  mkdir -p "$OUT/geo/$y"
  # One TopoJSON per province: neighbours share their borders, so the
  # simplification leaves no gaps between sections.
  $MAPSHAPER -i "$f" \
    -simplify 12% keep-shapes \
    -clean \
    -split p \
    -o "$OUT/geo/$y/" format=topojson quantization=100000 singles
done

# Provinces for the national map, from the latest year
latest=$(ls "$RAW"/geo-*.geojson | sort | tail -1)
$MAPSHAPER -i "$latest" -dissolve p copy-fields=p \
  -simplify 2% keep-shapes \
  -o "$OUT/geo/provinces.json" format=topojson quantization=100000

node scripts/build-derived.mjs "$RAW" "$OUT"
