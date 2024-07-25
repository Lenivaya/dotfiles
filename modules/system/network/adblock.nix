# Blocking some internet shit here
{ config, lib, ... }:
with lib;
with lib.my;
let
  cfg = config.modules.adblock;
in
{
  options.modules.adblock.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    networking.stevenblack = enabled // {
      block = [
        "gambling"
        "fakenews"
      ];
    };
  };
}
