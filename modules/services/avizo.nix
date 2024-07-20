{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  inherit (config.dotfiles) configDir;
  cfg = config.modules.services.avizo;
in {
  options.modules.services.avizo.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    home.configFile."avizo" = {
      source = "${configDir}/avizo";
      recursive = true;
    };

    environment.systemPackages = with pkgs; [avizo];
    # home.services.avizo = enabled;

    systemd.user.services.avizo = mkGraphicalService {
      description = "Notification daemon for volume and brightness";

      path = with pkgs; [avizo];
      script = "avizo-service";
    };
  };
}
