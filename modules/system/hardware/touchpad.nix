{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my;
let
  inherit (config.dotfiles) binDir;
  cfg = config.modules.hardware.touchpad;
in
{
  options.modules.hardware.touchpad.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    services.libinput = enabled // {
      touchpad = {
        disableWhileTyping = true;
        naturalScrolling = true;
        accelProfile = "adaptive";
      };
    };

    home.configFile."libinput-gestures.conf" = {
      text = ''
        gesture swipe right 3 ${binDir}/cycle_ws prev
        gesture swipe left 3 ${binDir}/cycle_ws next
        gesture swipe right 4 ${binDir}/cycle_windows prev
        gesture swipe left 4 ${binDir}/cycle_windows next

        gesture swipe up 4 wmctrl -r :ACTIVE: -b toggle,fullscreen
        gesture swipe up 3 skippy-xd --paging
        gesture swipe down 3 skippy-xd --expose
        gesture hold on 3 ${binDir}/zzz

        gesture pinch clockwise xdotool key alt+Tab
      '';
    };

    systemd.user.services.libinput-gestures = mkGraphicalService {
      description = "Touchpad gestures";

      path = with pkgs; [
        bash
        xdotool
        skippy-xd
        wmctrl
        procps
      ];
      script = getExe pkgs.libinput-gestures;

      serviceConfig = {
        Restart = "always";
      };
    };
  };
}
