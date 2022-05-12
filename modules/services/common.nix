{ config, pkgs, ... }: {
  services = {
    xbanish.enable = true;

    geoclue2.enable = true;
    acpid.enable = true;
    upower.enable = true;
    devmon.enable = true;
    gvfs.enable = true;
    udev.packages = [ pkgs.android-udev-rules ];
    openssh.enable = true;
    earlyoom.enable = true;

    gnome.gnome-keyring.enable = true;
  };
}
