{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.services.tailscale;
in
{
  options.modules.services.tailscale.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs.tailscale ];
    services.tailscale.enable = true;

    # Open required ports for Tailscale
    # https://tailscale.com/kb/1082/firewall-ports
    networking.firewall.allowedUDPPorts = [
      41641
      3478
    ];
  };
}
