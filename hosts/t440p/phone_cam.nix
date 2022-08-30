{
  config,
  lib,
  pkgs,
  ...
}: {
  boot.kernelModules = ["v4l2loopback"];
  boot.extraModprobeConfig = "options v4l2loopback exclusive_caps=1";
}
