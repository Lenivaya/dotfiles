{ pkgs, config, lib, ... }:

with lib.my;
let pragmata = config.modules.desktop.fonts.pragmatapro;
in {
  options.modules.desktop.fonts.pragmatapro = { enable = mkBoolOpt false; };

  config = {

    fonts = {
      fontDir.enable = true;
      enableDefaultFonts = true;
      enableGhostscriptFonts = true;
      fonts = with pkgs; [
        (if pragmata.enable then my.pragmatapro else null)
        iosevka
        ibm-plex
        noto-fonts
        ubuntu_font_family
        corefonts
        fira-code
        fira-code-symbols
      ];

      fontconfig = {
        enable = true;
        hinting = {
          autohint = false;
          enable = true;
        };
        subpixel.lcdfilter = "default";
        defaultFonts = {
          monospace = [ (if pragmata.enable then "PragmataPro Mono Liga" else "Iosevka") ];
          sansSerif = [ "IBM Plex Sans" ];
          serif = [ "IBM Plex Serif" ];
        };
      };
    };

  };
}
