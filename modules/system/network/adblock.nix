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

  config = mkIf cfg.enable {
    networking.stevenblack =
      enabled
      // {
        block = ["gambling" "fakenews"];
      };
  };
}
