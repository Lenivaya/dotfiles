{ config, options, lib, pkgs, ... }:

with lib;
let abstract_ring = pkgs.callPackage ./abstract_ring.nix { };
in {

  options.modules.bootAnimation = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf config.modules.bootAnimation.enable {
    boot.plymouth = {
      enable = true;
      theme = "abstract_ring";
      themePackages = with pkgs; [ abstract_ring ];
    };
  };
}
