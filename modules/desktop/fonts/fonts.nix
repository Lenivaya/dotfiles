{ pkgs, ... }:

{
  fonts = {
    fonts = with pkgs; [
      (iosevka.override {
        privateBuildPlan = {
          family = "Iosevka Term";
          design = [ "term" "ss08" ];
        };
        set = "term-ss08";
      })
      ibm-plex
      dejavu_fonts
      noto-fonts
      noto-fonts-emoji
      corefonts
      symbola
    ];

    fontconfig = {
      enable = true;
      subpixel.rgba = "rgb";
      antialias = true;
      hinting.enable = true;
      hinting.autohint = true;
      includeUserConf = true;
      localConf = ''
        <match target="font">
          <edit name="autohint" mode="assign">
            <bool>true</bool>
          </edit>
          <edit name="hinting" mode="assign">
            <bool>true</bool>
          </edit>
          <edit mode="assign" name="hintstyle">
            <const>hintslight</const>
          </edit>
          <edit mode="assign" name="lcdfilter">
           <const>lcddefault</const>
         </edit>
        </match>
      '';
      defaultFonts = {
        monospace = [ "Iosevka Term" ];
        sansSerif = [ "IBM Plex Sans" ];
        serif = [ "IBM Plex Serif" ];
      };
    };
  };
}
