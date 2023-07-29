{
  options,
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
  options.modules.services.avizo.enable = mkBoolOpt true;

  config = mkIf cfg.enable {
    home.configFile."avizo" = {
      source = "${configDir}/avizo";
      recursive = true;
    };

    environment.systemPackages = with pkgs; [avizo];

    systemd.user.services.avizo = {
      description = "Notification daemon for volume and brightness";
      wants = ["graphical-session.target"];
      wantedBy = ["graphical-session.target"];
      after = [
        "graphical-session.target"
        "display-manager.service"
        "skippy-xd.service"
      ];

      path = with pkgs; [avizo];
      script = "avizo-service";
    };
  };
}
