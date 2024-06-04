{ config, lib, pkgs, ... }:
let
  site = (import ../.).site;
  itcss = (import ../.).itcss;
  struct = (import ../.).struct;
  hs = (import ../.).hs;
  haddock = (import ../.).haddock;
  brochure = (import ../.).brochure;
  static = (import ../.).static;

  nginx-bits = import ../../nginx-bits;
  inherit (nginx-bits) blackhole-locations;
in
{
  services.nginx = {
    enable = true;
    package = pkgs.nginxMainline;
    additionalModules = [];
    recommendedGzipSettings = true;
    virtualHosts."hypered.design" = {
      locations = blackhole-locations // {
        "/" = {
          alias = site + "/";
        };
        "/echo".proxyPass = "http://127.0.0.1:8999";
        "/examples/" = {
          alias = itcss + "/examples/";
        };
        "/hs/" = {
          alias = hs + "/hs/";
        };
        "/haddock/" = {
          alias = haddock + "/share/doc/hypered-design-0.0.0/html/";
        };
        "/pdf/brochure/" = {
          alias = brochure + "/";
        };
        "/static/" = {
          alias = static + "/";
        };
        "/static/css/itcss/" = {
          alias = itcss + "/static/css/itcss/";
        };

        "=/struct.html" = {
          alias = struct + "/struct.html";
        };
        "/notes/" = {
          alias = struct + "/notes/";
        };
        "/specimens/" = {
          alias = struct + "/specimens/";
        };
        "/prototypes/" = {
          alias = struct + "/prototypes/";
        };
        "/static/css/struct/" = {
          alias = struct + "/static/css/struct/";
        };
      };
    };
  };
}
