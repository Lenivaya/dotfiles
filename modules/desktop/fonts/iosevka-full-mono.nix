{
  pkgs,
  config,
  lib,
  ...
}:
with lib;
with lib.my;
let
  inherit (config.modules.desktop.fonts) iosevka-full-mono;
in
{
  options.modules.desktop.fonts.iosevka-full-mono.enable = mkBoolOpt false;

  config = mkIf iosevka-full-mono.enable {
    fonts.packages = with pkgs; [ iosevka-bin ];

    fonts.fontconfig.defaultFonts = mkForce {
      monospace = [ "Iosevka" ];
      sansSerif = [ "Iosevka" ];
      serif = [ "Iosevka" ];
    };
  };
}
