{ config, lib, ... }:
with lib;
with lib.my;
let
  # inherit (config.boot.kernelPackages) x86_energy_perf_policy;
  cfg = config.modules.services.tlp;
in
{
  options.modules.services.tlp.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    services.tlp = enabled // {
      settings = {
        # Set CPU governors
        CPU_SCALING_GOVERNOR_ON_AC = "performance";
        CPU_SCALING_GOVERNOR_ON_BAT = "powersave";

        # Disable Turbo [Boost|Core] on battery
        CPU_BOOST_ON_AC = 1;
        CPU_BOOST_ON_BAT = 0;

        # Intel CPU HWP hints
        CPU_ENERGY_PERF_POLICY_ON_AC = "balance_performance";
        CPU_ENERGY_PERF_POLICY_ON_BAT = "balance_power";

        # Set CPU Frequency constraints
        CPU_MIN_PERF_ON_AC = 0;
        CPU_MAX_PERF_ON_AC = 100;
        CPU_MIN_PERF_ON_BAT = 0;
        CPU_MAX_PERF_ON_BAT = 30;

        # Use minimum amount of cores on battery
        SCHED_POWERSAVE_ON_AC = 0;
        SCHED_POWERSAVE_ON_BAT = 1;

        PLATFORM_PROFILE_ON_AC = "performance";
        PLATFORM_PROFILE_ON_BAT = "low-power";
        AHCI_RUNTIME_PM_ON_AC = "auto";
        AHCI_RUNTIME_PM_ON_BAT = "auto";
        PCIE_ASPM_ON_AC = "performance";
        PCIE_ASPM_ON_BAT = "powersupersave";
        RUNTIME_PM_ON_AC = "auto";
        RUNTIME_PM_ON_BAT = "auto";

        # Timeout (in seconds) for the audio power saving mode (supports Intel HDA, AC97).
        # A value of 1 is recommended for Linux desktop environments with PulseAudio,
        # systems without PulseAudio may require 10. The value 0 disables power save.
        SOUND_POWER_SAVE_ON_AC = 10;
        SOUND_POWER_SAVE_ON_BAT = 10;

        START_CHARGE_THRESH_BAT0 = 85;
        STOP_CHARGE_THRESH_BAT0 = 90;
        RESTORE_THRESHOLDS_ON_BAT = 1;

        DISK_IDLE_SECS_ON_BAT = 5;

        DEVICES_TO_DISABLE_ON_STARTUP = "bluetooth wwan";
        DEVICES_TO_ENABLE_ON_STARTUP = "wifi";

        # has precedence
        DEVICES_TO_ENABLE_ON_AC = "";
        DEVICES_TO_DISABLE_ON_BAT = "";

        DEVICES_TO_DISABLE_ON_BAT_NOT_IN_USE = "bluetooth wwan";

        DEVICES_TO_DISABLE_ON_LAN_CONNECT = "wifi wwan";
        DEVICES_TO_DISABLE_ON_WIFI_CONNECT = "";
        DEVICES_TO_DISABLE_ON_WWAN_CONNECT = "wifi";

        DEVICES_TO_ENABLE_ON_LAN_DISCONNECT = "wifi";
        DEVICES_TO_ENABLE_ON_WIFI_DISCONNECT = "";
        DEVICES_TO_ENABLE_ON_WWAN_DISCONNECT = "";

        DEVICES_TO_ENABLE_ON_DOCK = "wifi bluetooth";
        DEVICES_TO_ENABLE_ON_UNDOCK = "";
        DEVICES_TO_DISABLE_ON_UNDOCK = "bluetooth";
      };
    };

    # environment.systemPackages = [x86_energy_perf_policy];
    nixpkgs.overlays = [
      (_final: prev: {
        tlp = prev.tlp.override {
          enableRDW = true;
          # inherit # x86_energy_perf_policy
          #   enableRDW
          #   ;
        };
      })
    ];
  };
}
