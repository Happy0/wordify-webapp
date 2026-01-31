#!/bin/bash

cd ui/round

npm install
npm install --only=dev
npm run build

cd ../..

cp ui/wordify-views/dist/wordify.umd.js static/js/round.js
cp ui/wordify-views/dist/wordify-ui.css static/css/round.css
