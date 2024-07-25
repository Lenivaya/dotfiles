# yaml2nix
{ lib, ... }:
with lib;
with my;
{
  services.homepage-dashboard = enabled // {
    openFirewall = true;

    services = [
      {
        Media = [
          {
            Radarr = {
              description = "Movie manager";
              href = "http://local.self-hosted.com/radarr";
              icon = "radarr.png";
              siteMonitor = "http://local.self-hosted.com/radarr";
            };
          }
          {
            Readarr = {
              description = "Books manager";
              href = "http://local.self-hosted.com/readarr";
              icon = "readarr.png";
              siteMonitor = "http://local.self-hosted.com/readarr";
            };
          }
          {
            Sonarr = {
              description = "TV series";
              href = "http://local.self-hosted.com/sonarr";
              icon = "sonarr.png";
              siteMonitor = "http://local.self-hosted.com/sonarr";
            };
          }
          {
            Prowlarr = {
              href = "http://local.self-hosted.com/prowlarr";
              icon = "prowlarr.png";
              description = "Indexes Manager";
              siteMonitor = "http://local.self-hosted.com/prowlarr";
            };
          }
          {
            Calibre = {
              href = "http://local.self-hosted.com/calibre-web";
              icon = "calibre.png";
              description = "Calibre web interface";
              siteMonitor = "http://local.self-hosted.com/calibre-web";
            };
          }
          {
            Jellyfin = {
              description = "Media server";
              href = "http://local.self-hosted.com/jellyfin";
              icon = "jellyfin.png";
              siteMonitor = "http://local.self-hosted.com/jellyfin";
            };
          }
        ];
      }
      {
        Network = [
          {
            Adguard = {
              href = "http://local.self-hosted.com/aghome";
              icon = "adguard-home.png";
              siteMonitor = "http://local.self-hosted.com/aghome";
              description = "DNS Server and Ad Blocker";
              widget = {
                type = "adguard";
                url = "http://local.self-hosted.com/aghome";
              };
            };
          }
          {
            qBittorrent = {
              href = "http://local.self-hosted.com/qbt";
              icon = "qbittorrent.png";
              siteMonitor = "http://local.self-hosted.com/qbt";
              description = "Torrents";
            };
          }
        ];
      }
    ];
    bookmarks = [ ];
    settings = {
      headerStyle = "clean";
      statusStyle = "dot";
      hideVersion = true;
      cardBlur = "sm";
      language = "en";
      quicklaunch = {
        hideInternetSearch = true;
        hideVisitURL = true;
        searchDescriptions = true;
      };
    };
    widgets = [
      {
        resources = {
          cpu = true;
          memory = true;
          disk = "/";
        };
      }
    ];
  };
}
