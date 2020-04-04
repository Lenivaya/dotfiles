{ config, options, lib, pkgs, ... }:

with lib; {

  options.modules.services.kdeconnect = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf config.modules.services.kdeconnect.enable {
    # Open ports
    networking = {
      firewall.allowedTCPPortRanges = [{
        from = 1714;
        to = 1764;
      }];
    };

    my.home.services = { kdeconnect.enable = true; };
  };
}
