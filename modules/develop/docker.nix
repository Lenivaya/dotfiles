{
  lib,
  pkgs,
  config,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.dev.docker;
in {
  options.modules.dev.docker.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
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
      dockfmt
      # ctop
    ];
  };
}
