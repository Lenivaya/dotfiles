# https://askubuntu.com/questions/1379048/intel-pstate-driver-not-being-loaded-when-added-to-grub-file
# https://www.reddit.com/r/thinkpad/comments/12177bx/how_effective_is_charging_threshold_at_protecting/
{ lib, ... }:
with lib;
with my;
{
  services.tlp.settings = {
    CPU_ENERGY_PERF_POLICY_ON_AC = mkForce "performance";
    CPU_ENERGY_PERF_POLICY_ON_BAT = mkForce "power";

    CPU_SCALING_MAX_FREQ_ON_AC = MHz 4000;
    CPU_MAX_PERF_ON_BAT = mkForce 50;

    START_CHARGE_THRESH_BAT0 = 85;
    STOP_CHARGE_THRESH_BAT0 = 90;
    START_CHARGE_THRESH_BAT1 = 85;
    STOP_CHARGE_THRESH_BAT1 = 90;
  };

  # BUG https://github.com/AdnanHodzic/auto-cpufreq/issues/737
  # services.tlp = mkForce disabled;
  # # Superior power management for portable and battery powered systems. Plausible
  # # but unnecessary on desktop systems.
  # # See: <https://github.com/AdnanHodzic/auto-cpufreq>
  # services.auto-cpufreq = enabled // {
  #   settings = {
  #     charger = {
  #       # See available governors:
  #       #  `cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_available_governors`
  #       governor = "performance";
  #       # See available preferences:
  #       #  `cat /sys/devices/system/cpu/cpu0/cpufreq/energy_performance_available_preferences`
  #       energy_performance_preference = "performance";
  #       # scaling_min_freq = mkDefault (MHz 1700);
  #       scaling_max_freq = mkDefault (MHz 3600);
  #       turbo = "always";
  #     };

  #     battery = {
  #       governor = "powersave";
  #       energy_performance_preference = "power";
  #       # scaling_min_freq = mkDefault (MHz 1000);
  #       scaling_max_freq = mkDefault (MHz 1700);
  #       turbo = "never";

  #       enable_thresholds = true;
  #       start_threshold = 85;
  #       stop_threshold = 90;
  #     };
  #   };
  # };
}
