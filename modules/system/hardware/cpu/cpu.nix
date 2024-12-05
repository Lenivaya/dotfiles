{ config, lib, ... }:
with lib;
with lib.my;
let
  cfg = config.modules.hardware.cpu;
in
{
  options.modules.hardware.cpu = {
    tdp = {
      battery = {
        risky = mkBoolOpt false;
        updateRate = mkOption {
          type = types.int;
          default = 30;
        };

        p1 = {
          watts = mkOption {
            type = types.int;
            default = 0;
          };

          duration = mkOption {
            type = types.float;
            default = 0.0;
          };
        };

        p2 = {
          watts = mkOption {
            type = types.int;
            default = 0;
          };

          duration = mkOption {
            type = types.float;
            default = 0.0;
          };
        };

        cTDP = mkOption {
          type = types.int;
          default = 0;
          description = "Set cTDP to normal=0, down=1 or up=2 (EXPERIMENTAL)";
        };
      };
      ac = {
        risky = mkBoolOpt false;
        updateRate = mkOption {
          type = types.int;
          default = 5;
        };

        p1 = {
          watts = mkOption {
            type = types.int;
            default = 0;
          };

          duration = mkOption {
            type = types.float;
            default = 0.0;
          };
        };

        p2 = {
          watts = mkOption {
            type = types.int;
            default = 0;
          };

          duration = mkOption {
            type = types.float;
            default = 0.0;
          };
        };

        cTDP = mkOption {
          type = types.int;
          default = 0;
          description = "Set cTDP to normal=0, down=1 or up=2 (EXPERIMENTAL)";
        };
      };
    };

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
    services.undervolt =
      with cfg.undervolt;
      mkIf enable {
        coreOffset = core;
        gpuOffset = gpu;
        uncoreOffset = uncore;
        analogioOffset = analogio;
        inherit enable temp;
      };

    # Compatibility with throttled
    services.throttled.extraConfig =
      let
        # FIXME statix brokes this [1], wait until ignoring will be available [2]
        # [1]: https://github.com/nerdypepper/statix/issues/61#issuecomment-1606296495
        # [2]: https://github.com/nerdypepper/statix/issues/61
        mkValueString =
          v:
          if builtins.isFloat v then
            toString v
          else if true == v then
            "True"
          else if false == v then
            "False"
          else
            generators.mkValueStringDefault { } v;
        mkKeyValue = generators.mkKeyValueDefault { inherit mkValueString; } ":";
        toConf = generators.toINI { inherit mkKeyValue; };

        settings = {
          GENERAL = {
            Enabled = true;
            Sysfs_Power_Path = "/sys/class/power_supply/AC*/online";
            Autoreload = false;
          };

          BATTERY = {
            Update_Rate_s = 30;
            PL1_Tdp_W = cfg.tdp.battery.p1.watts;
            PL1_Duration_s = cfg.tdp.battery.p1.duration;
            PL2_Tdp_W = cfg.tdp.battery.p2.watts;
            PL2_Duration_S = cfg.tdp.battery.p2.duration;
            # Trip_Temp_C = 85;
            Trip_Temp_C = cfg.undervolt.temp;
            # Set cTDP to normal=0, down=1 or up=2 (EXPERIMENTAL)
            cTDP = cfg.tdp.battery.cTDP;
            # Disable BDPROCHOT (EXPERIMENTAL)
            Disable_BDPROCHOT = cfg.tdp.battery.risky;
          };

          AC = {
            Update_Rate_s = 5;
            PL1_Tdp_W = cfg.tdp.ac.p1.watts;
            PL1_Duration_s = cfg.tdp.ac.p1.duration;
            PL2_Tdp_W = cfg.tdp.ac.p2.watts;
            PL2_Duration_S = cfg.tdp.ac.p2.duration;
            # Trip_Temp_C = 95;
            Trip_Temp_C = cfg.undervolt.temp;
            # Set HWP energy performance hints to 'performance' on high load (EXPERIMENTAL)
            HWP_Mode = true;
            # Set cTDP to normal=0, down=1 or up=2 (EXPERIMENTAL)
            cTDP = cfg.tdp.ac.cTDP;
            # Disable BDPROCHOT (EXPERIMENTAL)
            Disable_BDPROCHOT = cfg.tdp.ac.risky;
          };

          # In theory, the undervolting can be more aggressive since the cpu isn't as stressed
          "UNDERVOLT.BATTERY" = settings."UNDERVOLT.AC";

          "UNDERVOLT.AC" = with cfg.undervolt; {
            CORE = core;
            GPU = gpu;
            CACHE = cache;
            UNCORE = uncore;
            ANALOGIO = analogio;
          };
        };
      in
      toConf settings;
  };
}
