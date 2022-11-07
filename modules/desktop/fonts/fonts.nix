{
  pkgs,
  config,
  lib,
  ...
}:
with lib.my; let
  inherit (config.modules.desktop.fonts) pragmata;
in {
  options.modules.desktop.fonts.pragmata.enable = mkBoolOpt false;

  config = {
    fonts = {
      fontDir.enable = true;
      enableDefaultFonts = true;
      enableGhostscriptFonts = true;
      fonts = with pkgs; [
        (
          if pragmata.enable
          then my.pragmatapro
          else iosevka
        )
        ibm-plex
        noto-fonts
        ubuntu_font_family
        corefonts
        merriweather
        julia-mono # Unicode glyphs

        # lmmath
        # lmodern
        cm_unicode
        # liberation_ttf

        # May be pretty nice
        # jetbrains-mono overpass
      ];

      fontconfig = {
        enable = true;
        hinting = {
          autohint = false;
          enable = true;
        };
        subpixel.lcdfilter = "default";
        defaultFonts =
          if pragmata.enable
          then {
            # Use pragmata everywhere when it's enabled (exactly what i want)
            monospace = ["PragmataPro Mono Liga"];
            sansSerif = ["PragmataPro Liga"];
            serif = ["PragmataPro Liga"];
          }
          else {
            monospace = ["Iosevka"];
            sansSerif = ["IBM Plex Sans"];
            serif = ["IBM Plex Serif"];
          };
      };
    };
  };
}
