{
  pkgs,
  config,
  lib,
  ...
}:
with lib;
with lib.my;
let
  inherit (config.modules.desktop.fonts) adwaita-pragmata;
in
{
  options.modules.desktop.fonts.adwaita-pragmata.enable = mkBoolOpt false;

  config = mkIf adwaita-pragmata.enable {
    fonts.packages = with pkgs; [
      pkgs.adwaita-pragmata
      my.pragmatapro
    ];

    fonts.fontconfig.defaultFonts = mkForce {
      monospace = [ "PragmataPro Mono Liga Regular" ];
      sansSerif = [ "Adwaita Sans" ];
      serif = [ "Adwaita Sans" ];
    };
  };
}
