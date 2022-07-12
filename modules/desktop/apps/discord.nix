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
  cfg = config.modules.desktop.apps.discord;
in {
  options.modules.desktop.apps.discord.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    nixpkgs.overlays = with inputs; [
      discord-overlay.overlay
      discocss.overlay
    ];

    user.packages = with pkgs; [
      # If not installed from unstable, Discord will sometimes soft-lock itself
      # on a "there's an update for discord" screen.

      discord
      discocss
    ];
  };
}
