{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  hwCfg = config.modules.hardware;
  cfg = hwCfg.bluetooth;
in {
  options.modules.hardware.bluetooth = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable (mkMerge [
    {
      # hardware.bluetooth = enabled // {package = pkgs.bluezFull;};
      hardware.bluetooth = enabled // {package = pkgs.bluez;};
      services.dbus.packages = [pkgs.blueman];
      services.blueman = enabled;

      environment.systemPackages = with pkgs; [
        overskride
        # blueberry
        bluetuith
      ];
    }

    (mkIf hwCfg.audio.enable {
      hardware.pulseaudio = {
        # NixOS allows either a lightweight build (default) or full build of
        # PulseAudio to be installed.  Only the full build has Bluetooth
        # support, so it must be selected here.
        package = pkgs.pulseaudioFull;
        # Enable additional codecs
        extraModules = [pkgs.pulseaudio-modules-bt];
        # Switch to bluetooth headset on connect
        extraConfig = ''
          load-module module-switch-on-connect
        '';
      };

      hardware.bluetooth.disabledPlugins = ["sap"];
      hardware.bluetooth.settings = {
        General = {
          MultiProfile = "multiple";
          FastConnectable = true;
          Privacy = "device";
          JustWorksRepairing = "always";

          Experimental = true;
          KernelExperimental = true;
          Enable = comcat [
            "Control"
            "Gateway"
            "Headset"
            "Media"
            "Sink"
            "Socket"
            "Source"
          ];
        };

        GATT = {KeySize = 16;};

        AVDTP = {
          SessionMode = "ertm";
          StreamMode = "streaming";
        };
      };

      home.services.mpris-proxy = enabled;
    })
  ]);
}
