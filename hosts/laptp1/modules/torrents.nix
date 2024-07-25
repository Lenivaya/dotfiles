{ lib, ... }:
with lib;
with my;
{
  services.qbittorrent = enabled // {
    openFirewall = true;
    group = "media";
  };
}
