{
  config,
  lib,
  pkgs,
  ...
}:
with lib.my; {
  virtualisation.docker = {
    enableOnBoot = false;
    enable = true;
  };

  user.extraGroups = ["docker"];
  user.packages = with pkgs; [docker-compose lazydocker docker-slim];
}
