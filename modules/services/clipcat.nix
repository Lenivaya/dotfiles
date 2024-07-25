# Simple clipboard manager to be integrated with rofi/dmenu
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

    home.configFile."clipcat" = {
      source = "${configDir}/clipcat";
      recursive = true;
    };
  };
}
