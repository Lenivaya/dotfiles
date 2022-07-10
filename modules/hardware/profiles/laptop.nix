{ config, pkgs, lib, ... }:

with lib;
with lib.my;
let cfg = config.modules.hardware.profiles.laptop;
in {
  options.modules.hardware.profiles.laptop = {
    enable = mkBoolOpt false;
    battery = mkOpt types.str "BAT0";
  };

  config = mkIf cfg.enable {
    boot.kernelParams = [
      # https://wiki.archlinux.org/index.php/improving_performance#Changing_I/O_scheduler
      "scsi_mod.use_blk_mq=1"
    ];
    fileSystems."/".options = [ "noatime" "nodiratime" ];

    powerManagement.enable = true;
    services.thermald.enable = true;

    services.irqbalance.enable = lib.mkDefault true;
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
  };
}
