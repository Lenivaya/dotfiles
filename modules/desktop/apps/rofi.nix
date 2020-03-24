{ config, lib, pkgs, ... }:

{
  my.home.programs = {
    rofi = {
      enable = true;
      lines = 10;
      scrollbar = true;
      cycle = true;
      theme = "gruvbox-dark-hard";
      extraConfig = ''
        rofi.modi: drun
      '';
    };
  };

}
