import html from "@rollup/plugin-html";

export default {
  build: {
    rollupOptions: {
      input: "./build/Tests.js",
    },
  },
  plugins: [
    html({
      title: "Vide Headless Tests",
    }),
  ],
};
