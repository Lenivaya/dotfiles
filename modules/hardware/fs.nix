{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.hardware.fs;
in {
  options.modules.hardware.fs = {
    enable = mkBoolOpt false;
    zfs.enable = mkBoolOpt false;
    ssd.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable (mkMerge [
    {
      # Auto-mount
      programs = {
        gnome-disks = enabled;
        udevil = enabled;
      };
      services.udisks2 = enabled;
      services.gvfs = enabled;

      # Support for more filesystems, mostly to support external drives
      environment.systemPackages = with pkgs; [
        sshfs
        exfat # Windows drives
        ntfs3g # Windows drives
        hfsprogs # MacOS drives
      ];
    }

    (mkIf (!cfg.zfs.enable && cfg.ssd.enable) {
      services.fstrim = enabled;
    })

    (mkIf cfg.zfs.enable (mkMerge [
      {
        boot.loader.grub.copyKernels = true;
        boot.supportedFilesystems = ["zfs"];
        boot.zfs.devNodes = "/dev/disk/by-partuuid";
        services.zfs.autoScrub = enabled;
      }

      (mkIf cfg.ssd.enable {
        # Will only TRIM SSDs; skips over HDDs
        services.fstrim = disabled;
        services.zfs.trim = enabled;
      })
    ]))
  ]);
}
