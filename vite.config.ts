import { defineConfig } from 'vite';

export default defineConfig({
  base: '/pansoft/',
  publicDir: 'static',
  optimizeDeps: {
    include: ['output/**/*.js'],
  },
});
