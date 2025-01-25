{
  pkgs,
  lib,
  config,
  ...
}:
with lib;
with lib.my;
{
  config = mkIf config.modules.desktop.enable {
    # https://www.reddit.com/r/linux_gaming/comments/16lwgnj/comment/k1536zb/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
    environment =
      let
        stemDarkening = "cff:no-stem-darkening=0 autofitter:no-stem-darkening=0";
      in
      {
        variables.FREETYPE_PROPERTIES = stemDarkening;
        sessionVariables.FREETYPE_PROPERTIES = stemDarkening;
      };

    fonts = {
      fontDir = enabled;
      enableDefaultPackages = true;
      enableGhostscriptFonts = true;

      packages = with pkgs; [
        ibm-plex
        noto-fonts
        corefonts
        merriweather
        nerd-fonts.symbols-only
        nerd-fonts.fira-code
        # (nerdfonts.override { fonts = [ "FiraCode" ]; })
        julia-mono # Unicode glyphs
        my.fast-font

        # (pkgs.nerdfonts.override { fonts = [ "NerdFontsSymbolsOnly" ]; })
        # lmmath
        # lmodern
        # cm_unicode
        # liberation_ttf
      ];

      fontconfig = enabled // {
        antialias = true;
        hinting = enabled // {
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
