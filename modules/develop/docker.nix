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
      enableOnBoot = false;
      enableNvidia =
        any (v: v == "nvidia")
        config.services.xserver.videoDrivers;
    };

  user.extraGroups = ["docker"];
  user.packages = with pkgs; [
    docker-compose
    lazydocker
    oxker
    docker-slim
  ];
}
