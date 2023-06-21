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
    audio.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable (mkMerge [
    {
      hardware.bluetooth =
        enabled
        // {
          package = pkgs.bluezFull;
        };
      services.dbus.packages = [pkgs.blueman];
      services.blueman = enabled;

      environment.systemPackages = with pkgs; [
        blueberry
        bluetuith
      ];
    }

    (mkIf cfg.audio.enable {
      hardware.pulseaudio = {
        # NixOS allows either a lightweight build (default) or full build of
        # PulseAudio to be installed.  Only the full build has Bluetooth
        # support, so it must be selected here.
        package = pkgs.pulseaudioFull;
        # Enable additional codecs
        extraModules = [pkgs.pulseaudio-modules-bt];
        # Switch to bluetooth headset on connect
        extraConfig = " load-module module-switch-on-connect ";
      };

      # hardware.bluetooth.settings = {
      #   General = {
      #     Enable = ["Control" "Gateway" "Headset" "Media" "Sink" "Socket" "Source"];
      #     Experimental = true;
      #     KernelExperimental = true;
      #   };
      # };

      user.services.mpris-proxy = enabled;
    })
  ]);
}
