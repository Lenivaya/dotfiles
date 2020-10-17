{ config, lib, pkgs, ... }:

with lib;
with lib.my; {
  config = {
    user.packages = with pkgs; [
      ranger
      ueberzug
    ];

    home.configFile."ranger" = {
      source = "${configDir}/ranger";
      recursive = true;
    };
  };
}
