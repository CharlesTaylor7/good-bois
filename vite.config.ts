import { defineConfig } from 'vite';

export default defineConfig({
  publicDir: 'public',
  optimizeDeps: {
    include: ['output/**/*.js'],
  },
});
