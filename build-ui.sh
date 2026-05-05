#!/bin/bash

set -e

cd ui/wordify-views

npm install
npm install --only=dev
npm run build

cd ../..

mkdir -p static/js static/css static/js/chunks

# Append a build timestamp to per-entry filenames for cache busting.
# Yesod's static file subsystem serves assets with aggressive far-future cache
# headers. Without this, the JS filename stays the same between builds, so
# browsers serve the old cached version until you do a shift+refresh.
# Including a timestamp in the filename (and updating the StaticRoutes in
# src/Settings/StaticFiles.hs) gives each build a unique URL that forces
# browsers to fetch the new version automatically.
TIMESTAMP=$(date +%Y%m%d_%H%M%S)

ENTRIES=(round create-game game-lobby game-invite home login choose-username)

# Clean old per-entry files (any previous timestamp), the shared CSS, and any
# leftovers from the pre-split single-bundle build.
for name in "${ENTRIES[@]}"; do
  rm -f "static/js/${name}_"*.js
  rm -f "static/js/${name}_"*.js.map
done
rm -f static/css/wordify-shared_*.css
rm -f static/js/wordify_*.js static/js/wordify_*.js.map static/css/wordify_*.css static/js/wordify.umd.js.map

# Shared chunks have content-hashed filenames so they're naturally
# cache-busted; remove the old set so we don't accumulate stale chunks.
rm -rf static/js/chunks
mkdir -p static/js/chunks

# Copy each entry's JS, applying the timestamp suffix. The entry's internal
# references to ./chunks/<file> remain valid because we mirror the chunks/
# subdirectory alongside.
for name in "${ENTRIES[@]}"; do
  cp "ui/wordify-views/dist/${name}.js" "static/js/${name}_${TIMESTAMP}.js"
  if [ -f "ui/wordify-views/dist/${name}.js.map" ]; then
    cp "ui/wordify-views/dist/${name}.js.map" "static/js/${name}_${TIMESTAMP}.js.map"
  fi
done

# Single shared CSS (cssCodeSplit is off in vite.config.ts, so the build emits
# one file containing primeicons + tailwind + base styles + per-view styles).
cp ui/wordify-views/dist/wordify-shared.css "static/css/wordify-shared_${TIMESTAMP}.css"

# Copy shared JS chunks. Their filenames already include content hashes from
# Vite, so they're cache-busted across builds without needing a timestamp.
if [ -d "ui/wordify-views/dist/chunks" ]; then
  cp -r ui/wordify-views/dist/chunks/. static/js/chunks/
fi

# Other assets (e.g., fonts emitted by tailwind/primeicons).
if [ -d "ui/wordify-views/dist/assets" ]; then
  mkdir -p static/assets
  cp -r ui/wordify-views/dist/assets/. static/assets/
fi

cp ui/wordify-views/dist/sw.js static/sw.js

# Update each per-entry filename in StaticFiles.hs
for name in "${ENTRIES[@]}"; do
  sed -i "s|\"js\", \"${name}_[_0-9]*\\.js\"|\"js\", \"${name}_${TIMESTAMP}.js\"|" src/Settings/StaticFiles.hs
done
sed -i "s|\"css\", \"wordify-shared_[_0-9]*\\.css\"|\"css\", \"wordify-shared_${TIMESTAMP}.css\"|" src/Settings/StaticFiles.hs
