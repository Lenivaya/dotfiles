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
      let
        discord' = wrapWithFlags "legcord" (getExe' pkgs.legcord "legcord") (
          spaceConcat config.modules.desktop.browsers.chromium.commandLineArgs
        );
      in
      [ discord' ];
  };
}
