{ config, lib, pkgs, ... }:

{
  imports = [ ./. ];

  boot.cleanTmpDir = true;

  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };

  hardware.enableRedistributableFirmware = true;
  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
    extraPackages = with pkgs; [
      vaapiIntel
      vaapiVdpau
      libvdpau-va-gl
      intel-media-driver
    ];
  };

  my.home.xdg.enable = true;
  environment.variables = {
    # These are the defaults, but some applications are buggy when these lack
    # explicit values.
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_CACHE_HOME  = "$HOME/.cache";
    XDG_DATA_HOME   = "$HOME/.local/share";
    XDG_BIN_HOME    = "$HOME/.local/bin";
  };
}
