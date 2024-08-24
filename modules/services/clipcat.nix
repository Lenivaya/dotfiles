# Simple clipboard manager to be integrated with rofi/dmenu
# FIXME BUG TODO https://github.com/xrelkd/clipcat/issues/434
{ config, lib, ... }:
with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  cfg = config.modules.services.clipcat;
in
{
  options.modules.services.clipcat.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    services.clipcat = enabled;
    systemd.user.services.clipcat.serviceConfig = {
      Restart = "on-failure";
    };

    home.configFile."clipcat" = {
      source = "${configDir}/clipcat";
      recursive = true;
    };
  };
}
