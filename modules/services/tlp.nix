{ ... }:

{
  services.tlp.enable = true;
  services.tlp.settings = {
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
  };
}
