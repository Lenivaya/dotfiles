{
  lib,
  config,
  ...
}:
with lib;
with lib.my; {
  # HACK Disable nvidia card for
  # the sake of power consumption
  # hardware.nvidiaOptimus.disable = true;
  # modules.hardware.gpu.nvidia = mkForce disabled;

  hardware = {
    nvidia = {
      open = false;
      package = config.boot.kernelPackages.nvidiaPackages.legacy_470;
      powerManagement.enable = mkForce false;
      powerManagement.finegrained = mkForce false;
      nvidiaPersistenced = mkForce false;
      prime = {
        reverseSync = enabled;
        offload.enableOffloadCmd = true;

        intelBusId = "PCI:0:2:0";
        nvidiaBusId = "PCI:2:0:0";
      };
    };
  };

  # https://www.reddit.com/r/archlinux/comments/gsq0ix/nvidia_drivers_ruins_my_resolution_and_dpi/
  services.xserver.config = lib.mkAfter ''
    Section "OutputClass"
        Identifier "nvidia dpi settings"
        MatchDriver "nvidia-drm"
        Option "UseEdidDpi" "False"
        Option "DPI" "96 x 96"
    EndSection
  '';

  specialisation = {
    on-the-go.configuration = {
      system.nixos.tags = ["on-the-go"];
      # HACK Disable nvidia card for
      # the sake of power consumption
      hardware.nvidiaOptimus.disable = true;
      modules.hardware.gpu.nvidia = mkForce disabled;
    };
  };
}
