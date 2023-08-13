{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.hardware.touchpad;

  inherit (config.dotfiles) configDir binDir;
in {
  options.modules.hardware.touchpad.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    services.xserver.libinput =
      enabled
      // {
        touchpad = {
          disableWhileTyping = true;
          naturalScrolling = true;
          accelProfile = "adaptive";
        };
      };

    home.configFile."libinput-gestures.conf" = {
      source = "${configDir}/libinput-gestures/libinput-gestures.conf";
      recursive = true;
    };

    systemd.user.services.libinput-gestures = mkGraphicalService {
      description = "Touchpad gestures";

      path = with pkgs; [
        xdotool
        skippy-xd

        wmctrl
        (writeShellScriptBin "toggleFullscreen"
          "wmctrl -r :ACTIVE: -b toggle,fullscreen")

        bash
        (writeShellScriptBin "next_ws" "${binDir}/cycle_ws next")
        (writeShellScriptBin "prev_ws" "${binDir}/cycle_ws prev")
      ];
      script = getExe pkgs.libinput-gestures;

      serviceConfig = {
        Restart = "always";
      };
    };
  };
}
