# https://git.kernel.org/pub/scm/network/wireless/iwd.git/tree/src/iwd.config.rst
{ lib, ... }:
with lib;
with my;
{
  networking.wireless.enable = mkForce false;
  networking.wireless.iwd = enabled // {
    settings = {
      Network = {
        EnableIPv6 = true;
      };
      Settings = {
        AutoConnect = true;
      };

      Rank.BandModifier5Ghz = 2.0; # prefer 5ghz

      # General.AddressRandomization = "once";
      # General.AddressRandomizationRange = "full";
    };
  };
  networking.networkmanager.wifi.backend = "iwd";
}
