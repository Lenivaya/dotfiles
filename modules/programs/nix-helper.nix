{ config, lib, ... }:
with lib;
with lib.my;
let
  cfg = config.modules.programs.nix-helper;
in
{
  options.modules.programs.nix-helper.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    programs.nh = enabled // {
      flake = config.dotfiles.dir';
    };

    # Removing diff script from srvos
    system.activationScripts.update-diff.text = mkForce "";

    # nixpkgs.overlays = [inputs.nh.overlays.default];
  };
}
