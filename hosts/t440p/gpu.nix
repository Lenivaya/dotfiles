{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; {
  # Enable nvidia gpu
  # hardware = {
  #   nvidia = {
  #     package = config.boot.kernelPackages.nvidiaPackages.legacy_470;
  #     modesetting = enabled;
  #     powerManagement.enable = lib.mkForce false;
  #     powerManagement.finegrained = lib.mkForce false;
  #     # nvidiaPersistenced = lib.mkForce false;
  #     prime = {
  #       sync = enabled;
  #       intelBusId = "PCI:0:2:0";
  #       nvidiaBusId = "PCI:2:0:0";
  #     };
  #   };
  # };
  # services.xserver.config = lib.mkAfter ''
  #   Section "OutputClass"
  #       Identifier "nvidia dpi settings"
  #       MatchDriver "nvidia-drm"
  #       Option "UseEdidDpi" "False"
  #       Option "DPI" "96 x 96"
  #   EndSection
  # '';

  hardware = {
    nvidia = {
      package = config.boot.kernelPackages.nvidiaPackages.legacy_470;
      modesetting = enabled;
      nvidiaPersistenced = mkForce false;
      powerManagement.enable = mkForce false;
      powerManagement.finegrained = mkForce false;
      # powerManagement = enabled;
      # powerManagement.finegrained = true;
      # nvidiaPersistenced = lib.mkForce false;
      prime = {
        offload = enabled;
        intelBusId = "PCI:0:2:0";
        nvidiaBusId = "PCI:2:0:0";
      };
    };
  };
  environment.systemPackages = let
    nvidia-offload = pkgs.writeShellScriptBin "nvidia-offload" ''
      export __NV_PRIME_RENDER_OFFLOAD=1
      export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0
      export __GLX_VENDOR_LIBRARY_NAME=nvidia
      export __VK_LAYER_NV_optimus=NVIDIA_only
      exec "$@"
    '';
  in [nvidia-offload];

  # HACK Disable nvidia card for
  # the sake of power consumption
  # hardware.nvidiaOptimus.disable = true;
}
