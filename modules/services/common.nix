{
  pkgs,
  lib,
  ...
}:
with lib.my; {
  services = {
    xbanish = enabled;

    geoclue2 = enabled;
    acpid = enabled;
    upower = enabled;
    devmon = enabled;
    udev.packages = [pkgs.android-udev-rules];
    openssh = enabled;
    earlyoom = enabled;

    gnome.gnome-keyring = enabled;

    # https://github.com/NixOS/nixpkgs/issues/135888
    nscd.enableNsncd = true;
  };
}
