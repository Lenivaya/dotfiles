{ pkgs, ... }:

{
  fonts = {
    fontDir.enable = true;
    enableDefaultFonts = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
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
        monospace = [ "Iosevka" ];
        sansSerif = [ "IBM Plex Sans" ];
        serif = [ "IBM Plex Serif" ];
      };
    };
  };
}
