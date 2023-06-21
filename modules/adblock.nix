# Blocking some internet shit here
{
  config,
  options,
  lib,
  inputs,
  ...
}:
with lib;
with lib.my; let
  inherit (inputs) adblock;

  cfg = config.modules.adblock;
in {
  options.modules.adblock.enable = mkBoolOpt false;

  imports = [adblock.nixosModule];

  config = mkIf cfg.enable {
    networking.stevenBlackHosts =
      enabled
      // {
        # blockFakenews = true;
        blockGambling = true;
        # blockPorn = true;
        # blockSocial = true;
      };
  };
}
