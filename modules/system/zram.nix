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
    zramSwap = enabled // {
      algorithm = "zstd";
      priority = 1000;
      memoryPercent = 90;
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
