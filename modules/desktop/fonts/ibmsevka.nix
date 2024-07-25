# Iosevka as monospace, IBM Plex as sans and serif
{
  pkgs,
  config,
  lib,
  ...
}:
with lib;
with lib.my;
let
  inherit (config.modules.desktop.fonts) ibmsevka;
in
{
  options.modules.desktop.fonts.ibmsevka.enable = mkBoolOpt false;

  config = mkIf ibmsevka.enable {
    fonts.packages = with pkgs; [
      iosevka-bin
      ibm-plex
    ];

    fonts.fontconfig.defaultFonts = mkForce {
      monospace = [ "Iosevka" ];
      sansSerif = [ "IBM Plex Sans" ];
      serif = [ "IBM Plex Serif" ];
    };
  };
}
