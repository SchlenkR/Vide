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
      tempalte: ({ attributes, bundle, files, publicPath, title }) => `
        <!DOCTYPE html>
        <html \${attributes}>
          <head>
            \${metas}
            <title>${title}</title>
            \${links}
          </head>
          <body>
            \${scripts}
          </body>
        </html>`,
    }),
  ],
};
