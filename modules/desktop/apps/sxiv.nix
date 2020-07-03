{ config, lib, pkgs, ... }:

{
  my.packages = with pkgs; [ sxiv ];

  my.home.xdg.configFile."sxiv" = {
    source = <config/sxiv>;
    recursive = true;
  };
}
