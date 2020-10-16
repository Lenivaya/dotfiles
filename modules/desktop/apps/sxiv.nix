{ config, lib, pkgs, ... }:

{
  my.packages = with pkgs; [ sxiv ];

  home.configFile."sxiv" = {
    source = <config/sxiv>;
    recursive = true;
  };
}
