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

  // Session cookie stored server-side, injected into all proxied requests.
  // This avoids document.cookie encoding issues.
  let storedSessionCookie = null;

  // Create Vite dev server in middleware mode for hot reloading
  const vite = await createViteServer({
    root: uiRoot,
    server: { middlewareMode: true },
    appType: 'custom',
  });

  // Helper to load and transform an HTML file through Vite
  async function renderPage(req, res, htmlFileName) {
    const htmlPath = resolve(__dirname, 'pages', htmlFileName);
    let html = fs.readFileSync(htmlPath, 'utf-8');
    html = await vite.transformIndexHtml(req.originalUrl, html);
    res.status(200).set({ 'Content-Type': 'text/html' }).end(html);
  }

  // Inject the stored session cookie into proxied requests
  function onProxyReq(proxyReq) {
    if (storedSessionCookie) {
      proxyReq.setHeader('Cookie', `_SESSION=${storedSessionCookie}`);
    }
  }

  // Proxy API and push requests to remote server.
  const apiProxy = createProxyMiddleware({
    target: remoteOrigin,
    changeOrigin: true,
    on: { proxyReq: onProxyReq },
    pathFilter: ['/api', '/push'],
  });
  app.use(apiProxy);

  // Proxy POST /games (create game) to remote server
  app.post('/games', createProxyMiddleware({
    target: remoteOrigin,
    changeOrigin: true,
    on: { proxyReq: onProxyReq },
  }));

  // Dev login: accept the session cookie via POST and store it server-side
  app.use(express.urlencoded({ extended: false }));

  app.post('/dev-login', (req, res) => {
    const cookie = req.body.session;
    if (!cookie) {
      return res.status(400).send('No session value provided');
    }
    storedSessionCookie = cookie;
    console.log('Session cookie stored on dev server.');
    res.redirect('/');
  });

  app.get('/dev-login', (req, res) => renderPage(req, res, 'dev-login.html'));

  // Catch /login requests from Vue components and redirect to dev login page
  app.get('/login', (_req, res) => {
    res.redirect('/dev-login');
  });

  // Use Vite's middleware for serving/transforming source files
  app.use(vite.middlewares);

  // HTML page routes
  app.get('/', (req, res) => renderPage(req, res, 'home.html'));
  app.get('/create-lobby', (req, res) => renderPage(req, res, 'create-game.html'));
  app.get('/games/:gameId/lobby', (req, res) => renderPage(req, res, 'game-lobby.html'));
  app.get('/games/:gameId', (req, res) => renderPage(req, res, 'round.html'));

  // Start server
  const server = app.listen(port, () => {
    console.log(`Dev server running at http://localhost:${port}`);
    console.log(`Proxying API/WebSocket requests to ${remoteOrigin}`);
    console.log('');
    console.log(`To log in: visit http://localhost:${port}/dev-login`);
  });

  // Proxy WebSocket upgrades to the remote server
  const wsProxy = createProxyMiddleware({
    target: remoteWs,
    changeOrigin: true,
    ws: true,
    on: { proxyReq: onProxyReq },
  });

  server.on('upgrade', (req, socket, head) => {
    // Inject session cookie into the WebSocket upgrade request
    if (storedSessionCookie) {
      req.headers.cookie = `_SESSION=${storedSessionCookie}`;
    }
    wsProxy.upgrade(req, socket, head);
  });
}

createDevServer();
