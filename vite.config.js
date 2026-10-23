import { defineConfig } from 'vite';

// Relative base: the same build runs on GitHub Pages (under /secciones-nacionalidades/)
// and inside the Android and iOS shells.
export default defineConfig({
  base: './',
  build: { outDir: 'dist', chunkSizeWarningLimit: 800 },
  test: { include: ['tests/web/**/*.test.js'] },
});
