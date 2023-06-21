{
  pkgs,
  lib,
  ...
}:
with lib.my; {
  fonts = {
    fontDir = enabled;
    enableDefaultFonts = true;
    enableGhostscriptFonts = true;

    fonts = with pkgs; [
      iosevka-bin
      ibm-plex
      noto-fonts
      ubuntu_font_family
      corefonts
      merriweather
      julia-mono # Unicode glyphs
      (nerdfonts.override {fonts = ["FiraCode"];})

      # lmmath
      # lmodern
      cm_unicode
      # liberation_ttf

      # May be pretty nice
      # jetbrains-mono overpass
    ];

    fontconfig =
      enabled
      // {
        hinting = enabled // {autohint = false;};
        subpixel.lcdfilter = "default";
        defaultFonts = {
          monospace = ["Iosevka"];
          sansSerif = ["IBM Plex Sans"];
          serif = ["IBM Plex Serif"];
        };
      };
  };
}
