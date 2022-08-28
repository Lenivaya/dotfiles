# Blocking some internet shit here
{
  config,
  options,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.adblock;
  inherit (inputs) adblock;
in {
  options.modules.adblock.enable = mkBoolOpt false;

  imports = [adblock.nixosModule];

  config = mkIf cfg.enable {
    networking.stevenBlackHosts = {
      enable = true;
      # blockFakenews = true;
      blockGambling = true;
      # blockPorn = true;
      # blockSocial = true;
    };
  };
}
