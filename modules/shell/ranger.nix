{ config, lib, pkgs, ... }:

{
  my.packages = with pkgs; [
    ranger
    (lib.mkIf config.services.xserver.enable w3m)
  ];

  my.home.xdg.configFile."ranger" = {
    source = <config/ranger>;
    recursive = true;
  };
}
