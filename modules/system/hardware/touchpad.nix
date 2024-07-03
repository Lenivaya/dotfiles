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
        bash
        xdotool
        skippy-xd

        wmctrl
        (writeShellScriptBin "toggleFullscreen"
          "wmctrl -r :ACTIVE: -b toggle,fullscreen")

        (writeShellScriptBin "cycle_ws" (readFile "${binDir}/cycle_ws"))
      ];
      script = getExe pkgs.libinput-gestures;

      serviceConfig = {
        Restart = "always";
      };
    };
  };
}
