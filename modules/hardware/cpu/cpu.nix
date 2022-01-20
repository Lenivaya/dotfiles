{ config, lib, ... }:

with lib;
with lib.my;

let cfg = config.modules.hardware.cpu;
in {
  options.modules.hardware.cpu = {

    undervolt = {
      enable = mkBoolOpt false;
      core = mkOption {
        type = types.int;
        default = 0;
      };

      gpu = mkOption {
        type = types.int;
        default = 0;
      };

      cache = mkOption {
        internal = true;
        type = types.int;
        default = 0;
      };

      uncore = mkOption {
        type = types.int;
        default = 0;
      };

      analogio = mkOption {
        type = types.int;
        default = 0;
      };

      temp = mkOption {
        type = types.int;
        default = 0;
      };

    };
  };

  config = {
    modules.hardware.cpu.undervolt.cache = cfg.undervolt.core;

    # Compatibility with intel-undervolt
    services.undervolt = with cfg.undervolt;
      mkIf enable {
        coreOffset = core;
        gpuOffset = gpu;
        uncoreOffset = uncore;
        analogioOffset = analogio;
        inherit enable;
        inherit temp;
      };

  };
}
