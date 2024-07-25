{ lib, ... }:
with lib;
{
  boot.loader.timeout = 1;

  networking = {
    networkmanager.enable = true;
    firewall.enable = true;
  };

  # hardware related stuff
  # nixpkgs.config.allowUnfree = true;
  hardware.enableRedistributableFirmware = true;
  hardware.enableAllFirmware = true;
  hardware.graphics = {
    enable = true;
  };

  services.automatic-timezoned.enable = true;

  console = {
    useXkbConfig = true;
  };

  i18n.defaultLocale = mkDefault "en_US.UTF-8";
  location.provider = "geoclue2";
}
