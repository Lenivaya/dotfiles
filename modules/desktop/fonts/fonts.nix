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
          defaultFonts = {
            monospace = ["Iosevka"];
            sansSerif = ["IBM Plex Sans"];
            serif = ["IBM Plex Serif"];
          };
        };
    };
  };
}
