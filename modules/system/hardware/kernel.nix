{lib, ...}:
with lib;
with lib.my; {
  boot = {
    # I don't use it even on laptops. It's also /required/ to disable it for
    # ZFS[1].
    # [1]: https://github.com/openzfs/zfs/issues/260
    # [1]: https://github.com/openzfs/zfs/issues/12842
    kernelParams = ["hibernate=no"];
  };

  # https://docs.kernel.org/admin-guide/mm/ksm.html
  hardware.ksm = enabled;
}
