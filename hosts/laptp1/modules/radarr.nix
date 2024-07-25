{ lib, ... }:
with lib;
with my;
{
  services.radarr = enabled // {
    openFirewall = true;
    group = "media";
  };
}
