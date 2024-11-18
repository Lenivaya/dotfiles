{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.desktop.apps.discord;
in
{
  options.modules.desktop.apps.discord.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    user.packages =
      with pkgs;
      let
        # discord' = vesktop;
        discord' = legcord;
      in
      [ discord' ];
  };
}
