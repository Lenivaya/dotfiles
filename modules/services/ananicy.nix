# https://github.com/brenix/nix-config/blob/main/modules/nixos/services/ananicy/default.nix
#
# When using any of the schedulers from the sched-ext framework,
# it’s strongly advised to disable and avoid using ananicy-cpp [1]
#
# [1]: https://wiki.cachyos.org/configuration/sched-ext/
{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with my;
let
  cfg = config.modules.services.ananicy;
in
{
  options.modules.services.ananicy = with types; {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    systemd.enableCgroupAccounting = true;

    services.ananicy = {
      enable = true;
      package = pkgs.ananicy-cpp;
      rulesProvider = pkgs.ananicy-rules-cachyos_git; # chaotic-nyx
      settings = {
        check_freq = 15;
        cgroup_load = true;
        type_load = true;
        rule_load = true;
        apply_nice = true;
        apply_latnice = true;
        apply_ioclass = true;
        apply_ionice = true;
        apply_sched = true;
        apply_oom_score_adj = true;
        apply_cgroup = true;
        check_disks_schedulers = true;

        loglelve = "critical";
        # If enabled it does log task name after rule matched and got applied to the task
        log_applied_rule = false;
      };
    };

    # ananicy spams the log constantly
    systemd.services.ananicy-cpp.serviceConfig.StandardOutput = "null";
  };
}
