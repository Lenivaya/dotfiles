{ config, lib, pkgs, ... }:

{
  # Open ports
  networking = {
    firewall.allowedTCPPortRanges = [{
      from = 1714;
      to = 1764;
    }];
  };

  my.home.services = { kdeconnect.enable = true; };
}
