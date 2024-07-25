{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  cfg = config.modules.desktop.apps.dunst;
in
{
  options.modules.desktop.apps.dunst.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    modules.desktop.apps.rofi = enabled;

    home.services.dunst = enabled;
    user.packages = with pkgs; [
      dunst
      libnotify
    ];

    home.configFile."dunst" = {
      source = "${configDir}/dunst";
      recursive = true;
    };
  };
}
