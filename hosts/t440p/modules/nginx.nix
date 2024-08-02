{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with my;
{
  networking.firewall.allowedTCPPorts = [
    80
    443
  ];

  services.nginx = enabled // {
    enableReload = true;
    package = pkgs.nginxMainline;
    statusPage = true;
    serverTokens = false;

    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;

    upstreams = {
      "aghome".servers."127.0.0.1:${toString config.services.adguardhome.port}" = { };
    };

    virtualHosts."local.self-hosted.com" =
      let
        withWebSockets = {
          proxyWebsockets = true;
        };
      in
      mkMerge [
        {
          default = true;

          locations."/aghome/" = {
            proxyPass = "http://aghome/";
            extraConfig = ''
              proxy_redirect / /aghome/;
              proxy_cookie_path / /aghome/;
            '';
          } // withWebSockets;
        }
      ];
  };
}
