{
  config,
  pkgs,
  lib,
  ...
}:
with lib;
with lib.my; let
  inherit (config.dotfiles) configDir;
  inherit (config) modules;

  cfg = config.modules.desktop.lockscreen;
  socket = "/tmp/xidlehook.sock";
in {
  options.modules.desktop.lockscreen.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    user.packages = with pkgs; [betterlockscreen xidlehook my.caffeinate];

    home.configFile."betterlockscreenrc" = {
      source = "${configDir}/betterlockscreen/betterlockscreenrc";
      recursive = true;
    };

    # home.services.xidlehook =
    #   enabled
    #   // {
    #     detect-sleep = true;
    #     not-when-audio = true;
    #     not-when-fullscreen = true;
    #   };

    systemd.user.services.xidlehook = mkGraphicalService {
      description = "General-purpose replacement for xautolock.";

      environment = {
        PRIMARY_DISPLAY = "$(xrandr | awk '/ primary/{print $1}')";
        XIDLEHOOK_SOCK = socket;
      };

      path = with pkgs; [betterlockscreen xorg.xrandr gawk];
      script = let
        execCommand = spaceConcat (
          [
            (getExe pkgs.xidlehook)
            "--detect-sleep"
            "--not-when-fullscreen"
            "--not-when-audio"
            "--socket ${socket}"
            "--timer 300 'betterlockscreen -l dim' ''"
          ]
          ++ optional modules.hardware.profiles.laptop.enable ''--timer 3600 "systemctl suspend" ""''
        );
      in
        execCommand;
    };
  };
}
