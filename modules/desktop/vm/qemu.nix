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
    virtualisation.libvirtd =
      enabled
      // {
        qemu = {
          package = pkgs.qemu_kvm;
          # runAsRoot = false;

          ovmf =
            enabled
            // {packages = [pkgs.OVMFFull.fd];};

          # Is this required for Windows 11?
          swtpm =
            enabled
            // {package = pkgs.swtpm-tpm2;};
        };
      };
    programs.dconf = enabled;

    environment.systemPackages = with pkgs; [
      qemu
      quickemu
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

