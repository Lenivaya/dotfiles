{
  options,
  config,
  lib,
  inputs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.programs.nix-helper;
in {
  options.modules.programs.nix-helper.enable = mkBoolOpt false;

  imports = [
    inputs.nh.nixosModules.default
  ];

  # env.FLAKE = config.dotfiles.dir;
  config = mkIf cfg.enable {
    nh =
      enabled
      // {
        clean =
          enabled
          // {
            dates = "weekly";
            extraArgs = "--keep-since 1w --keep 3";
          };
      };
  };
}
