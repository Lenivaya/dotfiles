{lib, ...}:
with lib; {
  boot.loader.timeout = 1;

  networking = {
    networkmanager.enable = true;
    firewall.enable = true;
  };

  # hardware related stuff
  nixpkgs.config.allowUnfree = true;
  hardware.enableRedistributableFirmware = true;
  hardware.enableAllFirmware = true;
  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
  };

  # time.timeZone = "Europe/Kiev";
  services.automatic-timezoned.enable = true;

  i18n.defaultLocale = mkDefault "en_US.UTF-8";
  location.provider = "geoclue2";
}
