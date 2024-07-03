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
      devmon = enabled;
      openssh = enabled;
      # earlyoom = enabled;

      # https://github.com/NixOS/nixpkgs/issues/135888
      nscd.enableNsncd = true;
    }

    (mkIf config.this.isHeadful {
      udev.packages = [pkgs.android-udev-rules];
    })

    (mkIf config.modules.desktop.enable {
      xbanish = enabled;
      gnome.gnome-keyring = enabled;
      dbus.implementation = "broker";
      geoclue2 = enabled;
    })
  ];
}
