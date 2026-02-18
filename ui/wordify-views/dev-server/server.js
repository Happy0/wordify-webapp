import express from 'express';
import { createServer as createViteServer } from 'vite';
import { createProxyMiddleware } from 'http-proxy-middleware';
import { fileURLToPath } from 'node:url';
import { dirname, resolve } from 'node:path';
import fs from 'node:fs';

const __dirname = dirname(fileURLToPath(import.meta.url));
const uiRoot = resolve(__dirname, '..');

function parseArgs() {
  const args = process.argv.slice(2);
  let hostname = null;
  for (let i = 0; i < args.length; i++) {
    if (args[i] === '--hostname' && args[i + 1]) {
      hostname = args[i + 1];
      break;
    }
  }
  if (!hostname) {
    console.error('Usage: node dev-server/server.js --hostname <remote-hostname>');
    console.error('Example: node dev-server/server.js --hostname wordify.example.com');
    process.exit(1);
  }
  return hostname;
}

async function createDevServer() {
  const hostname = parseArgs();
  const remoteOrigin = `https://${hostname}`;
  const remoteWs = `wss://${hostname}`;
  const app = express();
  const port = 3000;

  // Create Vite dev server in middleware mode for hot reloading
  const vite = await createViteServer({
    root: uiRoot,
    server: { middlewareMode: true },
    appType: 'custom',
  });

  // Helper to load and transform an HTML file through Vite
  async function renderPage(res, htmlFileName) {
    const htmlPath = resolve(__dirname, 'pages', htmlFileName);
    let html = fs.readFileSync(htmlPath, 'utf-8');
    // Vite transforms the HTML, resolving script imports and injecting HMR client
    html = await vite.transformIndexHtml('/', html);
    res.status(200).set({ 'Content-Type': 'text/html' }).end(html);
  }

  const proxyOptions = {
    target: remoteOrigin,
    changeOrigin: true,
    cookieDomainRewrite: { '*': '' },
  };

  // Proxy API requests to remote server (before vite middleware)
  app.use('/api', createProxyMiddleware(proxyOptions));
  app.use('/auth', createProxyMiddleware(proxyOptions));
  app.use('/login', createProxyMiddleware(proxyOptions));
  app.use('/push', createProxyMiddleware(proxyOptions));

  // Proxy POST /games (create game) to remote server
  app.post('/games', createProxyMiddleware(proxyOptions));

  // Use Vite's middleware for serving/transforming source files
  app.use(vite.middlewares);

  // HTML page routes
  app.get('/', (_req, res) => renderPage(res, 'home.html'));
  app.get('/create-lobby', (_req, res) => renderPage(res, 'create-game.html'));
  app.get('/games/:gameId/lobby', (_req, res) => renderPage(res, 'game-lobby.html'));
  app.get('/games/:gameId', (_req, res) => renderPage(res, 'round.html'));

  // Start server
  const server = app.listen(port, () => {
    console.log(`Dev server running at http://localhost:${port}`);
    console.log(`Proxying API/WebSocket requests to ${remoteOrigin}`);
  });

  // Proxy WebSocket upgrades to the remote server
  const wsProxy = createProxyMiddleware({
    target: remoteWs,
    changeOrigin: true,
    ws: true,
  });

  server.on('upgrade', (req, socket, head) => {
    wsProxy.upgrade(req, socket, head);
  });
}

createDevServer();
