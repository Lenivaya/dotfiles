{ ... }:
{
  imports = [
    ./adguard-home.nix
    ./radarr.nix
    ./sonarr.nix
    ./books.nix
    ./prowlarr.nix
    ./jellyfin.nix
    ./torrents.nix
    ./nginx.nix
    ./homepage/dashboard.nix
  ];
}
