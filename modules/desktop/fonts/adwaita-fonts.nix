{
  pkgs,
  config,
  lib,
  ...
}:
with lib;
with lib.my;
let
  inherit (config.modules.desktop.fonts) adwaita-fonts;
in
{
  options.modules.desktop.fonts.adwaita-fonts.enable = mkBoolOpt false;

  config = mkIf adwaita-fonts.enable {
    fonts.packages = with pkgs; [
      pkgs.adwaita-fonts
      nerd-fonts.adwaita-mono
    ];

    fonts.fontconfig.defaultFonts = mkForce {
      monospace = [ "Adwaita Mono" ];
      sansSerif = [ "Adwaita Sans" ];
      serif = [ "Adwaita Sans" ];
    };
  };
}
