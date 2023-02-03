{
  config,
  options,
  lib,
  pkgs,
  home-manager,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.browsers.chromium;
in {
  options.modules.desktop.browsers.chromium = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
    ungoogled = mkBoolOpt false;
    googled = mkBoolOpt false;
    flags = mkOption {
      type = types.listOf types.str;
      default = [];
    };
  };

  config = mkIf cfg.enable {
    home.programs.chromium = {
      enable = true;

      package = with pkgs;
        (
          if cfg.ungoogled
          then ungoogled-chromium
          else if cfg.googled
          then google-chrome
          else chromium
        )
        .override {commandLineArgs = lib.concatStringsSep " " cfg.flags;};

      extensions = [
        "cjpalhdlnbpafiamejdnhcphjbkeiagm" # ublock origin
        "pkehgijcmpdhfbdbbnkijodmdjhbjlgp" # privacy badger
        # "ldpochfccmkkmhdbclfhpagapcfdljkj" # decentraleyes
        "hfjbmagddngcpeloejdejnfgbamkjaeg" # vimium c
        "hkgfoiooedgoejojocmhlaklaeopbecg" # picture-in-picture
        "iaiomicjabeggjcfkbimgmglanimpnae" # tab sesssion manager
        "mmcgnaachjapbbchcpjihhgjhpfcnoan" # open new tab after current tab
        "nacjakoppgmdcpemlfnfegmlhipddanj" # pdf.js with vimium
        "bgfofngpplpmpijncjegfdgilpgamhdk" # modern scrollbar
        "opcjanmpjbdbdpnjfjbboacibokblbhl" # mut tab shortcuts
      ];
    };

    modules.desktop.browsers.chromium.flags = [
      # Dark theme
      "--force-dark-mode"
      # "--enable-features=WebUIDarkMode"

      # GPU acceleration
      "--ignore-gpu-blocklist"
      "--enable-gpu-rasterization"
      "--enable-native-gpu-memory-buffers"
      "--enable-zero-copy"
      # "--enable-features=VaapiVideoDecoder"
      # https://forum.manjaro.org/t/chromium-cant-enable-video-encoding-hardware-acceleration/101760/6
      "--use-gl=desktop"
      "--enable-oop-rasterization"
      "--enable-raw-draw"
      "--use-vulkan"
      "--disable-sync-preferences"
      "--enable-accelerated-2d-canvas"
      "--enable-accelerated-video-decode"
      "--enable-accelerated-mjpeg-decode"
      "--enable-features=VaapiVideoEncoder,VaapiVideoDecoder,CanvasOopRasterization"
      "--enable-gpu-compositing"

      # Fuck this "outdated" shit
      "--simulate-outdated-no-au='Tue, 31 Dec 2099 23:59:59 GMT'"
    ];
  };
}
