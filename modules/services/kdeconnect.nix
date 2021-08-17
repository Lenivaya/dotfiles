{ config, options, lib, pkgs, home-manager, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.kdeconnect;
in {
  options.modules.services.kdeconnect.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    # Open ports
    networking = {
      firewall.allowedTCPPortRanges = [{
        from = 1714;
        to = 1764;
      }];
    };

    home.services.kdeconnect.enable = true;
  };
}
