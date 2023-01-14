{ config, lib, pkgs, ... }:
let
  site = (import ../.).site;
  hs = (import ../.).hs;
  haddock = (import ../.).haddock;
  static = (import ../.).static;
in
{
  services.nginx = {
    enable = true;
    package = pkgs.nginxMainline;
    additionalModules = [];
    recommendedGzipSettings = true;
    virtualHosts."hypered.design" = {
      locations = {
        "/" = {
          alias = site + "/";
        };
        "/echo".proxyPass = "http://127.0.0.1:8999";
        "/hs/" = {
          alias = hs + "/hs/";
        };
        "/haddock/" = {
          alias = haddock + "/share/doc/design-system-0.0.0/html/";
        };
        "/static/" = {
          alias = static + "/";
        };
      };
    };
  };
}
