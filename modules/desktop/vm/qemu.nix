{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.vm.qemu;
in {
  options.modules.desktop.vm.qemu.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    boot.extraModprobeConfig = "options kvm_intel nested=1";
    virtualisation.libvirtd.enable = true;
    programs.dconf.enable = true;

    environment.systemPackages = with pkgs; [
      qemu
      virt-viewer
      virt-manager
      spice-vdagent
      bridge-utils
    ];

    user.extraGroups = ["libvirtd"];
  };
}
# Creating an image:
#   qemu-img create -f qcow2 disk.img
# Creating a snapshot (don't tamper with disk.img):
#   qemu-img create -f qcow2 -b disk.img snapshot.img

