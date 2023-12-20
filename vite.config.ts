import { defineConfig } from 'vite';

export default defineConfig({
  base: '/dog-breeds/',
  publicDir: 'static',
  optimizeDeps: {
    include: ['output/**/*.js'],
  },
});
