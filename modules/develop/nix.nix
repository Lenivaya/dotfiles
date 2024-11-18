{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.dev.nix;
in
{
  options.modules.dev.nix.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      nil
      nix-index
      nix-output-monitor
      nix-tree
      # nix-top

      niv
      cachix
      manix

      # nixpkgs-fmt
      nixfmt-rfc-style
      statix
      deadnix
    ];

    environment.shellAliases = {
      nixdev = "nix develop -c $SHELL";
      nixdevenv = "nix develop --impure -c $SHELL";
    };
  };
}
