{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.dev.nix;
in {
  options.modules.dev.nix.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      nix-index
      nix-output-monitor
      nix-tree

      niv
      cachix
      manix

      # nixpkgs-fmt
      # nixfmt
      alejandra
      statix
    ];
  };
}