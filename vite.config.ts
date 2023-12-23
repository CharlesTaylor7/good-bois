import { defineConfig } from 'vite';

export default defineConfig({
  base: "",
  publicDir: 'public',
  optimizeDeps: {
    include: ['output/**/*.js'],
  },
});
