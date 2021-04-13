{ config, lib, pkgs, ... }:

with lib; {
  boot.loader.timeout = 1;

  networking = {
    networkmanager.enable = true;
    firewall.enable = true;
  };


  # hardware related stuff
  hardware.enableRedistributableFirmware = true;
  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
    extraPackages = with pkgs; [
      vaapiVdpau
      libvdpau-va-gl
    ];
  };

  time.timeZone = "Europe/Kiev";
  i18n.defaultLocale = mkDefault "en_US.UTF-8";
  location.provider = "geoclue2";
}
