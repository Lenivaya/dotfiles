# I like some gnome apps
{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  inherit (config.dotfiles) configDir;

  cfg = config.modules.desktop.apps.gnome-circle;
in {
  options.modules.desktop.apps.gnome-circle.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      apostrophe
      blanket # ambient sounds
      wike
      # gnome-obfuscate # censor private informaton
    ];
  };
}
