const debug = process.env.NODE_ENV !== "production";

module.exports = {
  trailingSlash: true,
  exportPathMap: function() {
    return {
      "/": { page: "/" },
      "/components": { page: "/components" },
    };
  },
  assetPrefix: !debug ? "/design-system" : "",
};
