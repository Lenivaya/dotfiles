# https://askubuntu.com/questions/1379048/intel-pstate-driver-not-being-loaded-when-added-to-grub-file
{ lib, ... }:
with lib;
with my;
{
  services.tlp = mkForce disabled;

  # Superior power management for portable and battery powered systems. Plausible
  # but unnecessary on desktop systems.
  # See: <https://github.com/AdnanHodzic/auto-cpufreq>
  services.auto-cpufreq = {
    enable = true;
    settings = {
      charger = {
        # See available governors:
        #  `cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_available_governors`
        governor = "performance";
        # See available preferences:
        #  `cat /sys/devices/system/cpu/cpu0/cpufreq/energy_performance_available_preferences`
        energy_performance_preference = "performance";
        # scaling_min_freq = mkDefault (MHz 1700);
        scaling_max_freq = mkDefault (MHz 3400);
        turbo = "always";
      };

      battery = {
        governor = "schedutil";
        energy_performance_preference = "power";
        # scaling_min_freq = mkDefault (MHz 1000);
        scaling_max_freq = mkDefault (MHz 1700);
        turbo = "never";

        # Tresholds for battery, in percent. While those are useful to preserve battery life
        # e.g. to make your system battery live longer before you consider replacement, you
        # probably do not want those on a portable system.
        # enable_thresholds = true
        # start_threshold = 20
        # stop_threshold = 80
      };
    };
  };
}
