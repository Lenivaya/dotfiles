{
  config,
  options,
  lib,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.hardware.zram;
in {
  options.modules.hardware.zram.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    zramSwap =
      enabled
      // {
        algorithm = "zstd";
        memoryPercent = 25;
      };
  };
}
