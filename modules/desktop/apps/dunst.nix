{ config, pkgs, ...}:

{
  my.packages = with pkgs; [
    dunst libnotify
  ];

  my.home.xdg.configFile."dunst" = {
    source = <config/dunst>;
    recursive = true;
  };
}
