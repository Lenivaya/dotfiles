{ lib, ... }:
with lib;
with my;
{
  services.prowlarr = enabled // {
    openFirewall = true;
    # group = "media";
  };
}
