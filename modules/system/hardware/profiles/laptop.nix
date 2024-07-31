{
  config,
  pkgs,
  lib,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.hardware.profiles.laptop;
in
{
  options.modules.hardware.profiles.laptop = {
    enable = mkBoolOpt false;
    battery = mkOpt types.str "BAT0";
  };

  config = mkIf cfg.enable {
    modules = {
      hardware = {
        touchpad = enabled;
      };
      services = {
        tlp = enabled;
      };
    };

    services.upower = enabled;

    boot.kernelParams = [
      # https://wiki.archlinux.org/index.php/improving_performance#Changing_I/O_scheduler
      "scsi_mod.use_blk_mq=1"
    ];
    fileSystems."/".options = [
      "noatime"
      "nodiratime"
    ];

    powerManagement = enabled // {
      powertop = enabled;
    };
    # services.thermald = enabled;

    # # https://github.com/Irqbalance/irqbalance/issues/54#issuecomment-319245584
    # # https://unix.stackexchange.com/questions/710603/should-the-irqbalance-daemon-be-used-on-a-modern-desktop-x86-system
    #
    # Though there might be argument against it[1]:
    #
    # [1]: https://github.com/NixOS/nixpkgs/issues/299477#issuecomment-2023125360
    # services.irqbalance.enable = lib.mkDefault true;

    services.udev.extraRules = ''
      # Automatically suspend the system at <5%
      SUBSYSTEM=="power_supply", ATTR{status}=="Discharging", ATTR{capacity}=="[0-5]", RUN+="${pkgs.systemd}/bin/systemctl suspend"
      # Set scheduler for NVMe
      ACTION=="add|change", KERNEL=="nvme[0-9]*", ATTR{queue/scheduler}="none"
      # Set scheduler for SSD and eMMC
      ACTION=="add|change", KERNEL=="sd[a-z]|mmcblk[0-9]*", ATTR{queue/rotational}=="0", ATTR{queue/scheduler}="mq-deadline"
      # Set scheduler for rotating disks
      ACTION=="add|change", KERNEL=="sd[a-z]", ATTR{queue/rotational}=="1", ATTR{queue/scheduler}="bfq"
    '';

    systemd.user.timers.notify-on-low-battery = {
      timerConfig.OnBootSec = "2m";
      timerConfig.OnUnitInactiveSec = "2m";
      timerConfig.Unit = "notify-on-low-battery.service";
      wantedBy = [ "timers.target" ];
    };

    systemd.user.services.notify-on-low-battery = with cfg; {
      serviceConfig.PassEnvironment = "DISPLAY";
      script = ''
        export battery_capacity=$(${pkgs.coreutils}/bin/cat /sys/class/power_supply/${battery}/capacity)
        export battery_status=$(${pkgs.coreutils}/bin/cat /sys/class/power_supply/${battery}/status)

        if [[ $battery_capacity -le 10 && $battery_status = "Discharging" ]]; then
          ${pkgs.libnotify}/bin/notify-send --urgency=critical "$battery_capacity%: See you, space cowboy..."
        fi
      '';
    };

    systemd.timers."nh-clean".unitConfig.ConditionACPower = mkForce true;
    systemd.timers."logrotate".unitConfig.ConditionACPower = mkForce true;
    systemd.timers."fwupd-refresh".unitConfig.ConditionACPower = mkForce true;
    systemd.timers."docker-prune".unitConfig.ConditionACPower = mkForce true;
  };
}
