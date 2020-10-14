{ config, pkgs, lib, ... }:

with lib.my; {
  user.packages = with pkgs; [ betterlockscreen xidlehook ];

  home.configFile."betterlockscreenrc" = {
    source = <config/betterlockscreen/betterlockscreenrc>;
    recursive = true;
  };

  systemd.user.services.xidlehook = {
    description = "General-purpose replacement for xautolock.";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    serviceConfig = {
      ExecStart = ''
        ${pkgs.xidlehook}/bin/xidlehook \
          --not-when-fullscreen \
          --not-when-audio \
          --timer 300 "${pkgs.betterlockscreen}/bin/betterlockscreen -l dim" \
          ""
      '';
      Restart = "always";
      RestartSec = 2;
    };
  };
}
