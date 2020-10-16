{ config, lib, pkgs, ... }:

with lib.my; {
  my.packages = with pkgs; [ sxiv ];

  home.configFile."sxiv" = {
    source = "${configDir}/sxiv";
    recursive = true;
  };
}
