{ config, lib, pkgs, ... }:

with lib.my; {
  config = {
    user.packages = with pkgs; [
      ranger
      (lib.mkIf config.services.xserver.enable ueberzug)
    ];

    home.configFile."ranger" = {
      source = "${configDir}/ranger";
      recursive = true;
    };
  };
}
