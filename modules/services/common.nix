{
  pkgs,
  lib,
  config,
  ...
}:
with lib;
with lib.my; {
  services = mkMerge [
    {
      acpid = enabled;
      upower = enabled;
      devmon = enabled;
      udev.packages = [pkgs.android-udev-rules];
      openssh = enabled;
      # earlyoom = enabled;

      # https://github.com/NixOS/nixpkgs/issues/135888
      nscd.enableNsncd = true;
    }
    (mkIf config.modules.desktop.enable {
      xbanish = enabled;
      gnome.gnome-keyring = enabled;
      dbus.implementation = "broker";
      geoclue2 = enabled;
    })
  ];
}
