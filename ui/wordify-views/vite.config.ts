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
      entry: resolve(__dirname, 'src/lib.ts'),
      name: 'Wordify', // Global variable name for UMD
      fileName: (format) => `wordify.${format}.js`,
      formats: ['umd', 'es'] // UMD for script tags, ES for bundlers
    },
    rollupOptions: {
      // Don't externalize anything - bundle everything for standalone use
      external: [],
      output: {
        globals: {},
        exports: 'named'
      }
    },
    cssCodeSplit: false, // Bundle CSS into a single file
    sourcemap: true // Generate source maps for debugging original .ts/.vue files in browser DevTools
  }
})
