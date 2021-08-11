{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.hardware.gpu.nvidia;
in {
  options.modules.hardware.gpu.nvidia = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {

    boot.kernelModules =
      [ "nvidia" "nvidia_drm" "nvidia_uvm" "nvidia_modeset" ];

    environment.variables = {
      # Ultra low latency mode
      # https://devtalk.nvidia.com/default/topic/1067593/linux/how-to-turn-on-low-latency-mode-max-pre-render-frames-on-linux-/
      __GL_MaxFramesAllowed = "0";
      # Anisotropic texture filtering, 0-4, 0x, 2x, 4x, 8x, 16x
      __GL_LOG_MAX_ANISO = "4";
    };

    environment.systemPackages = with pkgs; [
      glxinfo
      cudnn
      cudatoolkit
      vdpauinfo

      # Respect XDG conventions, damn it!
      (writeScriptBin "nvidia-settings" ''
        #!${stdenv.shell}
        mkdir -p "$XDG_CONFIG_HOME/nvidia"
        exec ${config.boot.kernelPackages.nvidia_x11.settings}/bin/nvidia-settings --config="$XDG_CONFIG_HOME/nvidia/settings"
      '')
    ];

    hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.beta;
    hardware.nvidia.nvidiaPersistenced = true;
    hardware.nvidia.powerManagement.enable = true;
    hardware.nvidia.powerManagement.finegrained = true;
    hardware.opengl.enable = true;
    hardware.opengl.extraPackages = with pkgs; [ vaapiVdpau libvdpau-va-gl ];

    services.xserver.useGlamor = true;
    services.xserver.videoDrivers = [ "nvidia" ];

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
