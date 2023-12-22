import { defineConfig } from 'vite';

export default defineConfig({
  base: '/dog-breeds/',
  optimizeDeps: {
    include: ['output/**/*.js'],
  },
});
