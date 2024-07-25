{ lib, ... }:
with lib;
with my;
{
  services.sonarr = enabled // {
    openFirewall = true;
    group = "media";
  };
}
