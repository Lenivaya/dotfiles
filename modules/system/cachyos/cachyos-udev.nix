# https://github.com/CachyOS/CachyOS-Settings/tree/master/usr/lib/udev/rules.d
{ config, lib, ... }:
with lib;
with my;
let
  cfg = config.modules.cachyos.udev;
in
{
  options.modules.cachyos.udev.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    services.udev.extraRules = ''
      # SATA Active Link Power Management
      ACTION=="add", SUBSYSTEM=="scsi_host", KERNEL=="host*", \
          ATTR{link_power_management_policy}=="*", \
          ATTR{link_power_management_policy}="max_performance"

      # HDD
      ACTION=="add|change", KERNEL=="sd[a-z]*", ATTR{queue/rotational}=="1", \
          ATTR{queue/scheduler}="bfq"

      # SSD
      ACTION=="add|change", KERNEL=="sd[a-z]*|mmcblk[0-9]*", ATTR{queue/rotational}=="0", \
          ATTR{queue/scheduler}="mq-deadline"

      # NVMe SSD
      ACTION=="add|change", KERNEL=="nvme[0-9]*", ATTR{queue/rotational}=="0", \
          ATTR{queue/scheduler}="none"

      # Enable runtime PM for NVIDIA VGA/3D controller devices on driver bind
      ACTION=="add|bind", SUBSYSTEM=="pci", DRIVERS=="nvidia", \
          ATTR{vendor}=="0x10de", ATTR{class}=="0x03[0-9]*", \
          TEST=="power/control", ATTR{power/control}="auto"

      # Disable runtime PM for NVIDIA VGA/3D controller devices on driver unbind
      ACTION=="remove|unbind", SUBSYSTEM=="pci", DRIVERS=="nvidia", \
          ATTR{vendor}=="0x10de", ATTR{class}=="0x03[0-9]*", \
          TEST=="power/control", ATTR{power/control}="on"

      DEVPATH=="/devices/virtual/misc/cpu_dma_latency", OWNER="root", GROUP="audio", MODE="0660"

      KERNEL=="ntsync", MODE="0644"
    '';
  };
}
