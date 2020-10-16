{ config, options, lib, pkgs, ... }:

with lib;
let
  abstract_ring = pkgs.callPackage ./abstract_ring.nix { };
  cfg = config.modules.bootAnimation;
in {

  options.modules.bootAnimation = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    boot.plymouth = {
      enable = true;
      theme = "abstract_ring";
      themePackages = with pkgs; [ abstract_ring ];
    };
  };
}
