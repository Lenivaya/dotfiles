{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; {
  networking.firewall.allowedTCPPorts = [80 443];

  services.nginx =
    enabled
    // {
      enableReload = true;
      package = pkgs.nginxMainline;
      statusPage = true;
      serverTokens = false;

      recommendedGzipSettings = true;
      recommendedOptimisation = true;
      recommendedProxySettings = true;
      recommendedTlsSettings = true;

      upstreams."aghome" = {
        servers."127.0.0.1:3000" = {};
      };
      upstreams."dashboard" = {
        servers."127.0.0.1:${toString config.services.homepage-dashboard.listenPort}" = {};
      };
      upstreams."radarr" = {
        servers."127.0.0.1:7878" = {};
      };
      upstreams."readarr" = {
        servers."127.0.0.1:8787" = {};
      };
      upstreams."prowlarr" = {
        servers."127.0.0.1:9696" = {};
      };
      upstreams."jellyfin" = {
        servers."127.0.0.1:8096" = {};
      };
      upstreams."calibre" = {
        servers."127.0.0.1:${toString config.services.calibre-server.port}" = {};
      };
      upstreams."calibre-web" = {
        servers."127.0.0.1:${toString config.services.calibre-web.listen.port}" = {};
      };
      upstreams."qbittorrent" = {
        servers."127.0.0.1:${toString config.services.qbittorrent.port}" = {};
      };

      virtualHosts."local.self-hosted.com" = let
        mkArrProxy = arrService: arrServiceApiUrl: {
          locations."^~ /${arrService}" = {
            proxyPass = "http://${arrService}";
            extraConfig = ''
              proxy_redirect off;
              proxy_set_header Upgrade $http_upgrade;
              proxy_set_header Connection $http_connection;
            '';
          };
          locations."^~ /${arrServiceApiUrl}" = {
            proxyPass = "http://${arrService}";
            extraConfig = ''
              auth_basic off;
            '';
          };
        };
        mkArrProxy' = arrService: mkArrProxy arrService "${arrService}/api";
        withWebSockets = {
          proxyWebsockets = true;
        };
      in
        mkMerge [
          {
            default = true;

            locations."/" =
              {
                proxyPass = "http://dashboard";
              }
              // withWebSockets;

            locations."/aghome/" =
              {
                proxyPass = "http://aghome/";
                extraConfig = ''
                  proxy_redirect / /aghome/;
                  proxy_cookie_path / /aghome/;
                '';
              }
              // withWebSockets;

            locations."/qbt/" =
              {
                proxyPass = "http://qbittorrent/";
                extraConfig = ''
                  client_max_body_size 100M; # adding a lot of torrents at once
                  proxy_cookie_path  / "/; Secure";
                '';
              }
              // withWebSockets;

            locations."/calibre-web" =
              {
                proxyPass = "http://calibre-web";
                extraConfig = ''
                  client_max_body_size 1024M;
                  proxy_set_header X-Script-Name /calibre-web;
                '';
              }
              // withWebSockets;

            locations."/jellyfin" = {
              extraConfig = "return 302 $scheme://$host/jellyfin/;";
            };
            locations."/jellyfin/" =
              {
                proxyPass = "http://jellyfin";
                proxyWebsockets = true;
                extraConfig = ''
                  proxy_pass_request_headers on;
                  proxy_buffering off;
                '';
              }
              // withWebSockets;
          }
          (mkArrProxy' "radarr")
          (mkArrProxy' "readarr")
          (mkArrProxy "prowlarr" "prowlarr(/[0-9]+)?/api")
        ];
    };
}
