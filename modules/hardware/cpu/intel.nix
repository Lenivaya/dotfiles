{ config, pkgs, lib, inputs, ... }:

with lib;
with lib.my;
let cfg = config.modules.hardware.cpu.intel;
in {

  imports = [ inputs.external.nixosModules.sysfs ];
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
      "intel_iommu=on"
      "iommu=pt"

      "kvm_intel.nested=1"
      "kvm_intel.enable_apicv=1"
      # https://www.linux-kvm.org/images/e/e9/Kvm-forum-2013-nested-virtualization-shadow-turtles.pdf
      "kvm_intel.enlightened_vmcs=1"
      "kvm_intel.enable_shadow_vmcs=1"

      "snd-hda-intel.enable_msi=1"
    ];

    boot.kernel.sysfs.sys.devices.system.cpu = {
      # Enabling energy-efficiency optimizations may limit maximum operating frequency with or without the HWP feature.  With HWP enabled, the optimizations are done only in the turbo frequency range.  Without it, they are done in the entire available frequency range.
      intel_pstate.energy_efficiency = true;
      # It causes the minimum P-state limit to be increased dynamically for a short time whenever a task previously waiting on I/O is selected to run on a given logical CPU (the purpose of this mechanism is to improve performance).
      intel_pstate.hwp_dynamic_boost = true;
      # Minimum time (in microseconds) that has to pass between two consecutive runs of governor computations
      cpufreq.schedutil.rate_limit_us = 0;
    };

    environment.systemPackages = with pkgs; [
      pcm
      config.boot.kernelPackages.intel-speed-select
    ];

    hardware.cpu.intel.updateMicrocode = true;
    hardware.opengl.extraPackages = with pkgs; [ intel-ocl ];

    services.thermald.enable = true;
    # services.throttled.enable = lib.mkDefault true;
  };
}
