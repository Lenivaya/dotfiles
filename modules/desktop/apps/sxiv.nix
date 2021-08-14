{ config, lib, pkgs, ... }:

with lib.my;
let configDir = config.dotfiles.configDir;
in {
  config = {
    user.packages = with pkgs; [ sxiv ];

    home.configFile."sxiv" = {
      source = "${configDir}/sxiv";
      recursive = true;
    };
  };
}
