#!/bin/bash

cd ui/wordify-views

npm install
npm install --only=dev
npm run build

cd ../..

mkdir -p static/js static/css

cp ui/wordify-views/dist/wordify.umd.js static/js/wordify.js
cp ui/wordify-views/dist/wordify-ui.css static/css/wordify.css
cp ui/wordify-views/dist/wordify.umd.js.map static/js/wordify.umd.js.map
cp ui/wordify-views/dist/sw.js static/sw.js
