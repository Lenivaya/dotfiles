{
  config,
  lib,
  ...
}:
with lib.my; let
  inherit (config.boot.kernelPackages) x86_energy_perf_policy;
  enableRDW = config.networking.networkmanager.enable;
in {
  services.tlp =
    enabled
    // {
      settings = {
        # Set CPU governors
        CPU_SCALING_GOVERNOR_ON_AC = "schedutil";
        CPU_SCALING_GOVERNOR_ON_BAT = "schedutil";

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
      };
    };

  environment.systemPackages = [x86_energy_perf_policy];
  nixpkgs.overlays = [
    (_final: prev: {
      tlp = prev.tlp.override {
        inherit x86_energy_perf_policy enableRDW;
      };
    })
  ];
}
