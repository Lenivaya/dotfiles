{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.services.sxhkd;
in {
  options.modules.services.sxhkd = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home.configFile."sxhkd" = {
      source = "${configDir}/sxhkd";
      recursive = true;
    };

    systemd = {
      user.services.polkit-gnome-authentication-agent-1 = {
        description = "Sxhkd hotkeys";
        wants = ["graphical-session.target"];
        wantedBy = ["graphical-session.target"];
        after = ["graphical-session.target"];

        serviceConfig = {
          Type = "simple";
          ExecStart = "${pkgs.sxhkd}/bin/sxhkd -c ${configDir}/sxhkd/sxhkdrc";
          Restart = "on-failure";
          RestartSec = 1;
          TimeoutStopSec = 10;
        };
      };
    };
  };
}
