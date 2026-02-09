import { defineConfig } from 'vite'
import { resolve } from 'node:path'

export default defineConfig({
  build: {
    lib: {
      entry: resolve(__dirname, 'src/sw.ts'),
      formats: ['iife'],
      name: 'sw',
      fileName: () => 'sw.js'
    },
    outDir: 'dist',
    emptyOutDir: false,
    sourcemap: false
  }
})
