# Iosevka configured to look more like pragmata pro
{
  pkgs,
  config,
  lib,
  ...
}:
with lib;
with lib.my; let
  inherit (config.modules.desktop.fonts) pragmasevka;
in {
  options.modules.desktop.fonts.pragmasevka.enable = mkBoolOpt false;

  config = mkIf pragmasevka.enable {
    # fonts.fonts = with pkgs; [my.pragmasevka];
    fonts.packages = with pkgs; [my.pragmasevka];

    fonts.fontconfig.defaultFonts = mkForce {
      monospace = ["Pragmasevka"];
      sansSerif = ["Pragmasevka"];
      serif = ["Pragmasevka"];
    };
  };
}
