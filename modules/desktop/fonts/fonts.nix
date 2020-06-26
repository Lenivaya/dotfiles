{ pkgs, ... }:

{
  fonts = {
    enableDefaultFonts = true;
    fonts = with pkgs; [ iosevka ibm-plex noto-fonts corefonts symbola ];

    fontconfig = {
      enable = true;
      penultimate.enable = true;
      hinting = {
        autohint = false;
        enable = true;
      };
      subpixel.lcdfilter = "default";
      defaultFonts = {
        monospace = [ "Iosevka" ];
        sansSerif = [ "IBM Plex Sans" ];
        serif = [ "IBM Plex Serif" ];
      };
    };
  };
}
