{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [ binutils curl xclip xorg.xkill ];

  networking.networkmanager.enable = true;

  # Auto-mount
  programs = {
    gnome-disks.enable = true;
    udevil.enable = true;
  };

  # boot.cleanTmpDir = true;
  boot.tmpOnTmpfs = true;

  boot.loader = {
    timeout = 1;
    efi.canTouchEfiVariables = true;
    systemd-boot = {
      enable = true;
      editor = false;
      configurationLimit = 10;
    };
  };

  hardware.enableAllFirmware = true;
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
}
