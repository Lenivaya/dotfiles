{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let configDir = config.dotfiles.configDir;
in {
  config = {
    user.packages = with pkgs; [ ranger ueberzug ];

    home.configFile."ranger" = {
      source = "${configDir}/ranger";
      recursive = true;
    };
  };
}
