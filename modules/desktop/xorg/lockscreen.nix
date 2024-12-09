{
  config,
  pkgs,
  lib,
  ...
}:
with lib;
with lib.my;
let
  # inherit (config.dotfiles) configDir;
  # inherit (config) modules;

  cfg = config.modules.desktop.lockscreen;
  socket = "/tmp/xidlehook.sock";
in
{
  options.modules.desktop.lockscreen = {
    enable = mkBoolOpt false;
    autoSuspend = mkBoolOpt true;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      # betterlockscreen
      xidlehook
      my.caffeinate
    ];

    # home.configFile."betterlockscreen/betterlockscreenrc" = {
    #   source = "${configDir}/betterlockscreen/betterlockscreenrc";
    #   recursive = true;
    # };

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
        PRIMARY_DISPLAY = "$(xrandr | awk '/primary/{print $1}')";
        XIDLEHOOK_SOCK = socket;
      };

      path = with pkgs; [
        # betterlockscreen
        lightdm # FIXME XDG_SEAT_PATH is not present in systemd service context (/org/freedesktop/DisplayManager/Seat0)
        xorg.xrandr
        gawk
      ];
      script =
        let
          execCommand = spaceConcat (
            [
              (getExe pkgs.xidlehook)
              "--detect-sleep"
              "--not-when-fullscreen"
              "--not-when-audio"
              "--socket ${socket}"
              ''
                --timer 60 \
                  'xrandr --output "$PRIMARY_DISPLAY" --brightness .1' \
                  'xrandr --output "$PRIMARY_DISPLAY" --brightness 1' \
              ''
              # "--timer 200 'betterlockscreen -l dim' ''"
              "--timer 200 'dm-tool lock' ''"
            ]
            # ++ optional modules.hardware.profiles.laptop.enable ''--timer 3600 "systemctl suspend" ""''
            ++ optional cfg.autoSuspend ''--timer 200 "systemctl suspend" ""''
          );
        in
        execCommand;
    };
  };
}
