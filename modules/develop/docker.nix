{
  lib,
  pkgs,
  config,
  ...
}:
with lib;
with lib.my; {
  virtualisation.docker =
    enabled
    // {
      enableOnBoot = mkDefault false;
      autoPrune = enabled;
      enableNvidia = any (v: v == "nvidia") config.services.xserver.videoDrivers;
    };

  env = {
    DOCKER_CONFIG = "$XDG_CONFIG_HOME/docker";
    MACHINE_STORAGE_PATH = "$XDG_DATA_HOME/docker/machine";
  };

  user.extraGroups = ["docker"];
  user.packages = with pkgs; [
    docker
    docker-compose
    lazydocker
    oxker
    docker-slim
  ];
}
