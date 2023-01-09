const debug = process.env.NODE_ENV !== "production";

module.exports = {
  trailingSlash: true,
  exportPathMap: function() {
    return {
      "/landing": { page: "/landing" },
      "/components": { page: "/components" },
    };
  },
  assetPrefix: !debug ? "" : "",
};
