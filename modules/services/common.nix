{ config, pkgs, ... }: {
  services = {
    clight.enable = true;
    xbanish.enable = true;

    geoclue2.enable = true;
    thermald.enable = true;
    acpid.enable = true;
    upower.enable = true;
    devmon.enable = true;
    gvfs.enable = true;
    openssh.enable = true;
    earlyoom.enable = true;

    gnome3.gnome-keyring.enable = true;
  };
}
