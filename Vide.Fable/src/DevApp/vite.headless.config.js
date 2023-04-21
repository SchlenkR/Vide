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
        <html>
          <head>
          </head>
          <body>
            <button id="dummy>Dummy Button</button>
          </body>
        </html>`,
    }),
  ],
};
