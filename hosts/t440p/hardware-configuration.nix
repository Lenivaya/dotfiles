{
  modulesPath,
  config,
  lib,
  ...
}:
{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot.initrd.availableKernelModules = [
    "xhci_pci"
    "ehci_pci"
    "ahci"
    "usb_storage"
    "sd_mod"
    "sr_mod"
    "rtsx_pci_sdmmc"
  ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];
  boot.supportedFilesystems = [ "ntfs" ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/071e094d-4489-42fa-938a-4e936da0dbb3";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/2F3C-C998";
    fsType = "vfat";
  };

  fileSystems."/house" = {
    device = "/dev/disk/by-uuid/02A6D1CDA6D1C0F9";
    fsType = "ntfs";
    options = [
      "rw"
      "uid=1000"
    ];
  };

  swapDevices = [ { device = "/dev/disk/by-uuid/ed6d7da6-13ae-4025-95d4-2aba14752b3e"; } ];

  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  # networking.useDHCP = false;
  # networking.interfaces.enp0s25.useDHCP = true;
  # networking.interfaces.wlp4s0.useDHCP = true;
}
