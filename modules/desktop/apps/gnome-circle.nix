# I like some gnome apps
{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.desktop.apps.gnome-circle;
in
{
  options.modules.desktop.apps.gnome-circle.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      junction
      blanket # ambient sounds
      # wike
      metadata-cleaner
      gnome-obfuscate
      czkawka
      # pdfarranger
    ];
  };
}
