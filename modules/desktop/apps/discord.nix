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
      # discocss.overlay
    ];

    user.packages = with pkgs; let
      discord =
        # discord-plugged.override
        inputs.replugged.lib.makeDiscordPlugged
        {
          inherit pkgs;
          plugins = {
            inherit
              (inputs)
              discord-tweaks
              discord-image-tools
              discord-push-fix
              discord-better-status-indicators
              discord-multitask
              discord-view-raw
              discord-channel-typing
              ;
          };
          # themes = with inputs; [
          #   # discord-tokyonight
          #   # discord-crearts
          #   # discord-surcord
          # ];
        };
    in [
      # discord
      # discocss
      discord
      (writeScriptBin
        "discord"
        ''
          #!${stdenv.shell}
          exec discordcanary
        '')
    ];
  };
}
