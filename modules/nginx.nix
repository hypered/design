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
