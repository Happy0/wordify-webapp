#!/bin/bash

cd ui/round

npm install
npm run build

cd ../..

cp ui/round/dist/wordify.umd.js static/js/round.js
cp ui/round/dist/wordify-ui.css static/js/round.css
