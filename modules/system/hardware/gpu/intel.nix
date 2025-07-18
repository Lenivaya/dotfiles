{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.hardware.gpu.intel;
in
{
  options.modules.hardware.gpu.intel.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    services.xserver.videoDriver = mkDefault "modesetting";

    # let handle this on machine level config.
    # boot.kernelParams = [
    #   # Enable power-saving display C-states
    #   "i915.enable_dc=1"
    #   # Enable frame buffer compression for power savings
    #   "i915.enable_fbc=1"
    #   # Enable PSR
    #   "i915.enable_psr=1"
    #   # Display power wells are always on
    #   "i915.disable_power_well=0"
    #   # Enable GuC load for GuC submission and/or HuC load
    #   "i915.enable_guc=-1"
    #   # Enable GVT-g
    #   "i915.enable_gvt=1"
    #   # reserve the frame-buffer as setup by the BIOS or bootloader to avoid any flickering until Xorg
    #   "i915.fastboot=1"
    #   "i915.modeset=1"
    # ];

    environment.systemPackages = with pkgs; [
      intel-gpu-tools
      libva-utils
    ];

    hardware.graphics = enabled // {
      extraPackages =
        with pkgs;
        mkDefault [
          intel-media-driver # For Broadwell (2014) or newer processors. LIBVA_DRIVER_NAME=iHD
          vaapiIntel
          vaapiIntel
          vaapiVdpau
          libvdpau-va-gl
          intel-compute-runtime
          vpl-gpu-rt
        ];

      extraPackages32 =
        with pkgs.pkgsi686Linux;
        mkDefault [
          intel-media-driver
          vaapiIntel
        ];
    };
  };
}
