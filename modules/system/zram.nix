# https://wiki.archlinux.org/title/Zram
# https://github.com/pop-os/default-settings/blob/master_jammy/usr/bin/pop-zram-config
# https://github.com/pop-os/default-settings/blob/master_jammy/etc/default/pop-zram
{ config, lib, ... }:
with lib;
with lib.my;
let
  cfg = config.modules.zram;
in
{
  options.modules.zram = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {

    # NOTE Works but can't find parameters to tune algorithm.
    # zramSwap = enabled // {
    #   algorithm = mkDefault "zstd";
    #   memoryPercent = mkDefault 100;
    #   priority = mkDefault 1000;
    #   # memoryMax = megabytesToBytes 16384;
    # };

    services.zram-generator = enabled // {
      # https://github.com/CachyOS/CachyOS-Settings/blob/master/usr/lib/systemd/zram-generator.conf
      settings = {
        zram0 = {
          compression-algorithm = "zstd lz4 (type=huge)";
          zram-size = "ram";
          swap-priority = 100;
          fs-type = "swap";
        };
      };
    };

    # <https://www.kernel.org/doc/html/latest/admin-guide/sysctl/vm.html>
    # <https://github.com/pop-os/default-settings/pull/163>
    boot.kernel.sysctl = {
      # zram is relatively cheap, prefer swap
      #  swappiness refers to the kernel's willingness prefer swap
      #  over memory. higher values mean that we'll utilize swap more often
      #  which preserves memory, but will cause performance issues as well
      #  as wear on the drive
      "vm.swappiness" = 180; # 0-200
      # level of reclaim when memory is being fragmented
      "vm.watermark_boost_factor" = 0; # 0 to disable
      # aggressiveness of kswapd
      # it defines the amount of memory left in a node/system before kswapd is woken up
      "vm.watermark_scale_factor" = 125; # 0-300
      # zram is in memory, no need to readahead
      # page-cluster refers to the number of pages up to which
      # consecutive pages are read in from swap in a single attempt
      "vm.page-cluster" = 0;
    };
  };
}
