{
  pkgs,
  config,
  lib,
  ...
}:
with lib;
with lib.my;
let
  inherit (config.modules.desktop.fonts) iosevka;
in
{
  options.modules.desktop.fonts.iosevka.enable = mkBoolOpt false;

  config = mkIf iosevka.enable {
    fonts.packages = with pkgs; [
      iosevka-bin
      (iosevka-bin.override { variant = "Aile"; })
      (iosevka-bin.override { variant = "Etoile"; })
    ];

    fonts.fontconfig.defaultFonts = mkForce {
      monospace = [ "Iosevka" ];
      sansSerif = [ "Iosevka Aile" ];
      serif = [ "Iosevka Etoile" ];
    };
  };
}
