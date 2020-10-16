{ config, lib, pkgs, ... }:

with lib.my; {
  user.packages = with pkgs; [
    ranger
    (lib.mkIf config.services.xserver.enable ueberzug)
  ];

  home.configFile."ranger" = {
    source = <config/ranger>;
    recursive = true;
  };
}
