{ config, options, lib, ... }:

with lib;
let cfg = config.modules.hardware.zram;
in {
  options.modules.hardware.zram = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    zramSwap = {
      enable = true;
      algorithm = "lz4";
      memoryPercent = 25;
      numDevices = 1;
    };
  };
}
