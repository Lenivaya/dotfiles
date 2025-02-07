{
  pkgs,
  config,
  lib,
  ...
}:
with lib;
with lib.my;
let
  inherit (config.modules.desktop.fonts) pragmata;
in
{
  options.modules.desktop.fonts.pragmata.enable = mkBoolOpt false;

  config = mkIf pragmata.enable {
    fonts.packages = with pkgs; [ my.pragmatapro ];

    fonts.fontconfig.defaultFonts = mkForce {
      monospace = [ "PragmataPro Mono Liga Regular" ];
      sansSerif = [ "PragmataPro Liga Regular" ];
      serif = [ "PragmataPro Liga Regular" ];
    };
  };
}
