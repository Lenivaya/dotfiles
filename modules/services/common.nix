{ config, pkgs, ... }: {
  services = {
    clight.enable = true;
    xbanish.enable = true;

    geoclue2.enable = true;
    thermald.enable = true;
    acpid.enable = true;
    tlp.enable = true;
    upower.enable = true;
    devmon.enable = true;
    gvfs.enable = true;
    openssh.enable = true;
    # earlyoom.enable = true;
  };
}
