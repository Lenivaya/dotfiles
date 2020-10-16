{ config, lib, pkgs, ... }:

with lib.my; {
  config = {
    user.packages = with pkgs; [ sxiv ];

    home.configFile."sxiv" = {
      source = "${configDir}/sxiv";
      recursive = true;
    };
  };
}
