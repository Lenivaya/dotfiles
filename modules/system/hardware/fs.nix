{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.hardware.fs;
in
{
  options.modules.hardware.fs = {
    enable = mkBoolOpt false;
    zfs.enable = mkBoolOpt false;
    ssd.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable (mkMerge [
    {
      # Auto-mount
      programs = {
        udevil = enabled;
      };
      services.udisks2 = enabled;

      # Support for more filesystems, mostly to support external drives
      environment.systemPackages = with pkgs; [
        sshfs
        exfat # Windows drives
        ntfs3g # Windows drives
        hfsprogs # MacOS drives
      ];
    }

    (mkIf config.modules.desktop.enable {
      programs = {
        gnome-disks = enabled;
      };
      services.gvfs = enabled;
    })

    (mkIf (!cfg.zfs.enable && cfg.ssd.enable) {
      services.fstrim = mkDefault enabled;

      # tweak fstim service to run only when on AC power
      # and to be nice to other processes
      # (this is a good idea for any service that runs periodically)
      systemd.services.fstrim = {
        unitConfig.ConditionACPower = true;

        serviceConfig = {
          Nice = 19;
          IOSchedulingClass = "idle";
        };
      };
    })

    (mkIf cfg.zfs.enable (mkMerge [
      {
        boot.loader.grub.copyKernels = true;
        boot.supportedFilesystems = [ "zfs" ];
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
