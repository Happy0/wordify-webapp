import { defineConfig } from 'vite'
import vue from '@vitejs/plugin-vue'
import tailwindcss from '@tailwindcss/vite'
import { fileURLToPath, URL } from 'node:url'
import { resolve } from 'node:path'

// https://vite.dev/config/
export default defineConfig({
  plugins: [vue(), tailwindcss()],
  resolve: {
    alias: {
      '@': fileURLToPath(new URL('./src', import.meta.url))
    }
  },
  define: {
    'process.env.NODE_ENV': JSON.stringify('production')
  },
  build: {
    lib: {
      entry: {
        round: resolve(__dirname, 'src/lib/round.ts'),
        'create-game': resolve(__dirname, 'src/lib/create-game.ts'),
        'game-lobby': resolve(__dirname, 'src/lib/game-lobby.ts'),
        'game-invite': resolve(__dirname, 'src/lib/game-invite.ts'),
        home: resolve(__dirname, 'src/lib/home.ts'),
        login: resolve(__dirname, 'src/lib/login.ts'),
        'choose-username': resolve(__dirname, 'src/lib/choose-username.ts')
      },
      formats: ['es']
    },
    rollupOptions: {
      external: [],
      output: {
        entryFileNames: '[name].js',
        chunkFileNames: 'chunks/[name]-[hash].js',
        assetFileNames: (assetInfo) => {
          if (assetInfo.name?.endsWith('.css')) {
            // The build merges everything (primeicons + tailwind + global styles
            // + per-view component styles) into one file because cssCodeSplit is
            // off — most of the CSS is global anyway. Give it a stable name so
            // build-ui.sh can pick it up by filename.
            return 'wordify-shared[extname]'
          }
          return 'assets/[name]-[hash][extname]'
        }
      }
    },
    cssCodeSplit: false,
    sourcemap: true
  }
})
