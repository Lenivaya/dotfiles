{
  config,
  lib,
  options,
  pkgs,
  ...
}:
with lib;
with my; {
  # TODO FIXME
  # https://github.com/NixOS/nixpkgs/issues/315574#issuecomment-2137120500
  # devtunnel
  options.system.nixos.codeName = mkOption {readOnly = false;};
  config.system.nixos.codeName = mkForce "Vicuna";
}
