{ config, lib, ... }:
with lib;
with lib.my;
let
  cfg = config.modules.services.tailscale;
in
{
  options.modules.services.tailscale.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    # make the tailscale command usable to users
    environment.systemPackages = [ pkgs.tailscale ];

    # enable the tailscale service
    services.tailscale.enable = true;
  };
}
