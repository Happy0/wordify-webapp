#!/bin/bash

cd ui/wordify-views

npm install
npm install --only=dev
npm run build

cd ../..

mkdir -p static/js static/css

# Append a build timestamp to the JS filename for cache busting.
# Yesod's static file subsystem serves assets with aggressive far-future cache
# headers. Without this, the JS filename stays the same between builds, so
# browsers serve the old cached version until you do a shift+refresh.
# Including a timestamp in the filename (and updating the StaticRoute in
# src/Settings/StaticFiles.hs) gives each build a unique URL that forces
# browsers to fetch the new version automatically.
TIMESTAMP=$(date +%Y%m%d_%H%M%S)

rm -f static/js/wordify*.js
rm -f static/css/wordify*.css
cp ui/wordify-views/dist/wordify.umd.js "static/js/wordify_${TIMESTAMP}.js"
cp ui/wordify-views/dist/wordify-ui.css "static/css/wordify_${TIMESTAMP}.css"
cp ui/wordify-views/dist/wordify.umd.js.map static/js/wordify.umd.js.map
cp ui/wordify-views/dist/sw.js static/sw.js

# Update the Haskell static routes to point to the new timestamped filenames
sed -i "s|\"js\", \"wordify[_0-9]*\.js\"|\"js\", \"wordify_${TIMESTAMP}.js\"|" src/Settings/StaticFiles.hs
sed -i "s|\"css\", \"wordify[_0-9]*\.css\"|\"css\", \"wordify_${TIMESTAMP}.css\"|" src/Settings/StaticFiles.hs
