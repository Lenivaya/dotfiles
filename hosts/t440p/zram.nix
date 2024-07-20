{lib, ...}:
with lib;
with my; {
  zramSwap =
    enabled
    // {
      algorithm = "zstd";
      priority = 100;
      memoryPercent = 90;
    };

  boot.kernel.sysctl = {
    # ZRAM is relatively cheap, prefer swap
    "vm.swappiness" = 180;
    # ZRAM is in memory, no need to readahead
    "vm.page-cluster" = 0;

    # level of reclaim when memory is being fragmented
    "vm.watermark_boost_factor" = 0;
    # aggressiveness of kswapd
    # it defines the amount of memory left in a node/system before kswapd is woken up
    "vm.watermark_scale_factor" = 125;
  };
}
