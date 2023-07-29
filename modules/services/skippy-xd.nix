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
  cfg = config.modules.services.skippy-xd;
in {
  options.modules.services.skippy-xd.enable = mkBoolOpt true;

  config = mkIf cfg.enable {
    home.configFile."skippy-xd" = {
      source = "${configDir}/skippy-xd";
      recursive = true;
    };

    environment.systemPackages = with pkgs; [skippy-xd];

    systemd.user.services.skippy-xd = {
      description = "Windows and workspaces selector";
      wants = ["graphical-session.target"];
      wantedBy = ["graphical-session.target"];
      after = [
        "graphical-session.target"
        "display-manager.service"
      ];

      path = with pkgs; [skippy-xd];
      script = "skippy-xd --start-daemon";
    };
  };
}
