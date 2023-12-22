import { defineConfig } from 'vite';

export default defineConfig({
  base: '/dog-breeds/',
  assetsInclude: "**/*.gif",
  publicDir: 'public',
  optimizeDeps: {
    include: ['output/**/*.js'],
  },
});
