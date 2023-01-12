{
  config,
  options,
  lib,
  pkgs,
  ...
}: {
  modules.dev = {
    nix.enable = true;
  };
}
