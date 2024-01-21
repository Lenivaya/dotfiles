{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.services.tray;

  spawnCommand = command: "${command} &";
in {
  options.modules.services.tray = with types; {
    enable = mkBoolOpt false;
    trayer = mkBoolOpt false;
    trayApps = mkOpt (listOf str) [
      "blueman-applet"
      "nm-applet"
      "pasystray"
      "mictray"
      "udiskie -t"
      "kdeconnect-indicator"
    ];
  };

  config = mkIf (cfg.enable && config.services.xserver.enable) {
    # fake a tray to let apps start
    # https://github.com/nix-community/home-manager/issues/2064
    # systemd.user.targets.tray = {
    #   Unit = {
    #     Description = "Home Manager System Tray";
    #     Requires = ["graphical-session-pre.target"];
    #   };
    # };

    systemd.user.services.trayer = mkGraphicalService {
      enable = cfg.trayer;

      description = "X tray for WM's";

      serviceConfig = {
        Type = "forking";
        RemainAfterExit = true;
      };

      path = with pkgs; [trayer];
      script = let
        trayerCommand = spaceConcat [
          "trayer"
          "-l"
          "--edge top --align center"
          "--distancefrom top --distance 100"
          "--SetDockType true --SetPartialStrut false"
          "--widthtype request --expand true"
          "--transparent true --alpha 255"
          "--height 26 --heighttype pixel"
          "--iconspacing 5"
        ];
      in
        spawnCommand trayerCommand;
    };

    systemd.user.services.trayApps = mkGraphicalService {
      description = "tray apps";
      after = ["tray.service"];

      serviceConfig = {
        Type = "forking";
        RemainAfterExit = true;
        Restart = "always";
      };

      path = with pkgs; [
        networkmanagerapplet
        blueman
        pasystray
        mictray
        udiskie
        kdeconnect
        cbatticon
      ];
      script = let
        trayElements = spaceConcat (map spawnCommand cfg.trayApps);
      in
        trayElements;
    };
  };
}
