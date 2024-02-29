import { defineConfig } from 'vite';

export default defineConfig({
  base: '/good-bois/',
  optimizeDeps: {
    include: ['output/**/*.js'],
  },
});
