{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.hardware.cpu.intel;
in
{
  options.modules.hardware.cpu.intel.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    boot.kernelModules = [
      "kvm_intel"

      "isst_if_mmio"
      "isst_if_common"
      "isst_if_mbox_msr"
      "isst_if_mbox_pci"
    ];

    boot.kernelParams = [
      # Enable power-saving display C-states
      "i915.enable_dc=1"
      # Enable frame buffer compression for power savings
      "i915.enable_fbc=1"
      # Enable PSR
      "i915.enable_psr=1"
      # Display power wells are always on
      "i915.disable_power_well=0"
      # Enable GuC load for GuC submission and/or HuC load
      "i915.enable_guc=-1"

      "intel_iommu=on"
      "iommu=pt"

      "kvm_intel.nested=1"
      "kvm_intel.enable_apicv=1"
      # https://www.linux-kvm.org/images/e/e9/Kvm-forum-2013-nested-virtualization-shadow-turtles.pdf
      "kvm_intel.enlightened_vmcs=1"
      "kvm_intel.enable_shadow_vmcs=1"

      "snd-hda-intel.enable_msi=1"
    ];

    hardware.cpu.intel.updateMicrocode = true;

    hardware.opengl.extraPackages = with pkgs; [
      (vaapiIntel.override { enableHybridCodec = true; })
      intel-media-driver
      intel-ocl
      intel-compute-runtime
    ];

    environment.systemPackages = with pkgs; [
      pcm
      config.boot.kernelPackages.intel-speed-select
      libva-utils
    ];
  };

}
