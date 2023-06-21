{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.apps.discord;
in {
  options.modules.desktop.apps.discord.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    # Consider armcord client
    user.packages = with pkgs; let
      pkg = discord.override {
        withOpenASAR = true;
        withVencord = true;
      };
    in [pkg];
  };
}
