{ pkgs, ... }:
{
  services.nginx = {
    virtualHosts."hypered.design" = {
      forceSSL = true;
      enableACME = true;
    };
  };

  security.acme.acceptTerms = true;
  security.acme.certs = {
    "hypered.design".email = "noteed@gmail.com";
  };
}
