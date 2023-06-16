{lib, ...}:
with lib.my; {
  boot = {
    # I don't use it even on laptops. It's also /required/ to disable it for
    # ZFS[1].
    # [1]: https://github.com/openzfs/zfs/issues/260
    # [1]: https://github.com/openzfs/zfs/issues/12842
    kernelParams = ["hibernate=no"];

    kernel.sysctl = {
      "fs.file-max" = pow 2 17;
      "fs.inotify.max_user_watches" = pow 2 19;
      "fs.suid_dumpable" = 0;
      "kernel.core_uses_pid" = 1;
      "kernel.exec-shield" = 1;
      "kernel.kptr_restrict" = 1;
      "kernel.maps_protect" = 1;
      "kernel.msgmax" = pow 2 16;
      "kernel.msgmnb" = pow 2 16;
      "kernel.pid_max" = pow 2 16;
      "kernel.randomize_va_space" = 2;
      "kernel.shmall" = pow 2 28;
      "kernel.shmmax" = pow 2 28;
      "kernel.sysrq" = 0;
      "vm.dirty_background_bytes" = pow 2 22;
      "vm.dirty_background_ratio" = 5;
      "vm.dirty_bytes" = pow 2 22;
      "vm.dirty_ratio" = 30;
      "vm.min_free_kbytes" = pow 2 16;
      "vm.mmap_min_addr" = pow 2 12;
      "vm.overcommit_memory" = mkDefault 0;
      "vm.overcommit_ratio" = mkDefault 50;
      "vm.vfs_cache_pressure" = 50;
    };
  };

  # https://docs.kernel.org/admin-guide/mm/ksm.html
  hardware.ksm.enable = true;
}
