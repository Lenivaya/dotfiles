{ config, options, lib, ... }:
with lib;
let cfg = config.modules.zram;
in {
  options.modules.zram = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    zramSwap = {
      enable = true;
      algorithm = "zstd";
      memoryPercent = 25;
      numDevices = 1;
      priority = 100;
    };
  };
}
