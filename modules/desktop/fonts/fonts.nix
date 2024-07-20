{
  pkgs,
  lib,
  config,
  ...
}:
with lib;
with lib.my; {
  config = mkIf config.modules.desktop.enable {
    fonts = {
      fontDir = enabled;
      enableDefaultPackages = true;
      enableGhostscriptFonts = true;

      packages = with pkgs; [
        ibm-plex
        noto-fonts
        # ubuntu_font_family
        corefonts
        merriweather
        julia-mono # Unicode glyphs
        (pkgs.nerdfonts.override {fonts = ["NerdFontsSymbolsOnly"];})
        # (nerdfonts.override {fonts = ["FiraCode"];})

        # lmmath
        # lmodern
        # cm_unicode
        # liberation_ttf
      ];

      fontconfig =
        enabled
        // {
          antialias = true;
          hinting =
            enabled
            // {
              autohint = false;
              style = "slight";
            };
          subpixel = {
            lcdfilter = "default";
            rgba = "rgb";
          };
        };
    };
  };
}
