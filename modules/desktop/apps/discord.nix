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
        # discord' = discord.override {
        #   withOpenASAR = true;
        #   withVencord = true;
        #   nss = pkgs.nss_latest;
        # };
        discord' = vesktop;
      in
      [ discord' ];
  };
}
