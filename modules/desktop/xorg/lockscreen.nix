{ config, pkgs, lib, ... }:

with lib.my; {
  user.packages = with pkgs; [ betterlockscreen xidlehook ];

  home.configFile."betterlockscreenrc" = {
    source = "${configDir}/betterlockscreen/betterlockscreenrc";
    recursive = true;
  };

  systemd.user.services.xidlehook = {
    description = "General-purpose replacement for xautolock.";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];

    path =
      let
        locker = with pkgs;
          (writeScriptBin "locker" ''
            #!${stdenv.shell}
            ${pkgs.betterlockscreen}/bin/betterlockscreen -l dim
          '');
      in
      [ locker ];

    environment = {
      XIDLEHOOK_SOCK = "%t/xidlehook.socket";
    };

    serviceConfig = {
      ExecStart = ''
        ${pkgs.xidlehook}/bin/xidlehook \
          --detect-sleep \
          --not-when-fullscreen \
          --not-when-audio \
          --socket "$XIDLEHOOK_SOCK" \
          --timer 300 locker ""
      '';
      Restart = "always";
      RestartSec = 2;
    };
  };
}
