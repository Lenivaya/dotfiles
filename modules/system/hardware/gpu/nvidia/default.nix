{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.hardware.gpu.nvidia;
in
{
  options.modules.hardware.gpu.nvidia = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    # blacklist nouveau module so that it does not conflict with nvidia drm stuff
    # also the nouveau performance is godawful, I'd rather run linux on a piece of paper than use nouveau
    # no offense to nouveau devs, I'm sure they're doing their best and they have my respect for that
    # but their best does not constitute a usable driver for me
    boot.blacklistedKernelModules = [ "nouveau" ];

    boot.kernelModules = [
      "nvidia"
      "nvidia_drm"
      "nvidia_uvm"
      "nvidia_modeset"
    ];

    services.xserver.videoDrivers = [ "nvidia" ];

    environment.variables = {
      # Ultra low latency mode
      # https://devtalk.nvidia.com/default/topic/1067593/linux/how-to-turn-on-low-latency-mode-max-pre-render-frames-on-linux-/
      __GL_MaxFramesAllowed = "0";
      # Anisotropic texture filtering, 0-4, 0x, 2x, 4x, 8x, 16x
      __GL_LOG_MAX_ANISO = "4";
    };

    environment.sessionVariables = {
      LIBVA_DRIVER_NAME = mkDefault "nvidia";
    };

    environment.systemPackages = with pkgs; [
      glxinfo
      # cudaPackages.cudnn
      # cudaPackages.cuda_compat
      # cudatoolkit
      vdpauinfo

      # vulkan
      vulkan-tools
      vulkan-loader
      vulkan-validation-layers
      vulkan-extension-layer

      # libva
      libva
      libva-utils
    ];

    hardware.nvidia = {
      modesetting = enabled;
      nvidiaSettings = false; # add nvidia-settings to pkgs, useless on nixos

      nvidiaPersistenced = true;
      powerManagement = {
        enable = mkDefault true;
        finegrained = mkDefault false;
      };
    };

    hardware.graphics = enabled // {
      extraPackages = with pkgs; [
        nvidia-vaapi-driver
        vaapiVdpau
        libvdpau-va-gl
      ];
      extraPackages32 = with pkgs.pkgsi686Linux; [ nvidia-vaapi-driver ];
      enable32Bit = true;
    };

    boot.kernelParams = [
      # If the Spectre V2 mitigation is necessary, some performance may be recovered by setting the
      # NVreg_CheckPCIConfigSpace kernel module parameter to 0. This will disable the NVIDIA driver's
      # sanity checks of GPU PCI config space at various entry points, which were originally required
      # to detect and correct config space manipulation done by X server versions prior to 1.7.
      "nvidia.NVreg_CheckPCIConfigSpace=0"
      # Enable the PAT feature [5], which affects how memory is allocated. PAT was first introduced in
      # Pentium III [6] and is supported by most newer CPUs (see wikipedia:Page attribute table#Processors).
      # If your system can support this feature, it should improve performance.
      "nvidia.NVreg_UsePageAttributeTable=1"
    ];
  };
}
