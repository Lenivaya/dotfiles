{ config, lib, ... }:
with lib;
with my;
let
  cfg = config.modules.cachyos.settings;
in
{
  options.modules.cachyos.settings.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    # https://github.com/CachyOS/CachyOS-Settings/blob/master/usr/lib/sysctl.d/99-cachyos-settings.conf
    boot.kernel.sysctl = {
      # Memory management settings
      "vm.swappiness" = mkDefault 100;
      "vm.vfs_cache_pressure" = mkForce 50;
      "vm.dirty_bytes" = mkForce 268435456;
      "vm.page-cluster" = mkDefault 0;
      "vm.dirty_background_bytes" = mkForce 67108864;
      "vm.dirty_writeback_centisecs" = mkForce 1500;

      # Kernel settings
      "kernel.nmi_watchdog" = mkForce 0;
      "kernel.unprivileged_userns_clone" = mkForce 1;
      "kernel.printk" = mkForce "3 3 3 3";
      "kernel.kptr_restrict" = mkForce 2;
      "kernel.kexec_load_disabled" = mkForce 1;

      # Network settings
      "net.core.netdev_max_backlog" = mkForce 4096;

      # Filesystem settings
      "fs.file-max" = mkForce 2097152;
      "fs.xfs.xfssyncd_centisecs" = mkForce 10000;
    };

    # https://github.com/CachyOS/CachyOS-Settings/blob/master/usr/lib/tmpfiles.d/thp-shrinker.conf
    boot.kernel.sysfs.sys.kernel.mm.transparent_hugepage.khugepaged.max_ptes_none = 409;

    # https://github.com/CachyOS/CachyOS-Settings/blob/master/usr/lib/tmpfiles.d/thp-shrinker.conf
    boot.kernel.sysfs.sys.kernel.mm.transparent_hugepage.defrag = "defer+madvise";
  };
}
