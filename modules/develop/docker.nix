{ config, lib, pkgs, ... }:

with lib.my; {
  virtualisation.docker.enableOnBoot = false;
  virtualisation.docker.enable = true;
  user.extraGroups = [ "docker" ];
  user.packages = with pkgs; [ docker-compose_2 ];
}
