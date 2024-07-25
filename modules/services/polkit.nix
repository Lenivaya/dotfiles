# GUI for polkit
{
  pkgs,
  lib,
  config,
  ...
}:
with lib;
with lib.my;
{
  config = mkIf config.modules.desktop.enable {
    security.polkit = enabled;

    # systemd = {
    #   # user.services.polkit-gnome-authentication-agent-1 = {
    #   #   description = "polkit-gnome-authentication-agent-1";
    #   #   wants = ["graphical-session.target"];
    #   #   wantedBy = ["graphical-session.target"];
    #   #   after = ["graphical-session.target"];

    #   #   serviceConfig = {
    #   #     Type = "simple";
    #   #     ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
    #   #     Restart = "on-failure";
    #   #     RestartSec = 1;
    #   #     TimeoutStopSec = 10;
    #   #   };
    #   # };
    # };

    # systemd.user.services.polkit-pantheon-authentication-agent-1 = mkGraphicalService {
    #   script = "${pkgs.pantheon.pantheon-agent-polkit}/libexec/policykit-1-pantheon/io.elementary.desktop.agent-polkit";
    # };

    systemd.user.services.polkit-gnome-authentication-agent-1 = mkGraphicalService {
      script = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
    };
  };
}
