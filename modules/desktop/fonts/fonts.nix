{ pkgs, ... }:

{
  fonts = {
    enableDefaultFonts = true;
    fonts = with pkgs; [
      (iosevka.override {
        privateBuildPlan = {
          family = "Iosevka Term";
          design = [ "term" "ss08" ];
        };
        set = "term-ss08";
      })
      ibm-plex
      noto-fonts
      corefonts
      symbola
    ];

    fontconfig = {
      enable = true;
      penultimate.enable = true;
      hinting = {
        autohint = false;
        enable = true;
      };
      subpixel.lcdfilter = "default";
      defaultFonts = {
        monospace = [ "Iosevka Term" ];
        sansSerif = [ "IBM Plex Sans" ];
        serif = [ "IBM Plex Serif" ];
      };
    };
  };
}
