{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.browsers.chromium;
  pkg = with pkgs; (
    if cfg.ungoogled
    then ungoogled-chromium
    else if cfg.googled
    then google-chrome
    else chromium
  );
in {
  options.modules.desktop.browsers.chromium = with types; {
    enable = mkBoolOpt false;
    ungoogled = mkBoolOpt false;
    googled = mkBoolOpt false;
    flags = mkOpt (listOf str) [
      # Dark theme
      "--force-dark-mode"
      # "--enable-features=WebUIDarkMode"

      # GPU acceleration
      "--ignore-gpu-blocklist"
      "--enable-gpu-rasterization"
      "--enable-native-gpu-memory-buffers"
      "--enable-zero-copy"
      # "--enable-features=VaapiVideoDecoder"
      # # https://forum.manjaro.org/t/chromium-cant-enable-video-encoding-hardware-acceleration/101760/6
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

  config = mkIf cfg.enable {
    home.programs.chromium =
      enabled
      // {
        package = let
          commandLineArgs = spaceConcat cfg.flags;
        in
          pkg.override {inherit commandLineArgs;};

        extensions = [
          "cjpalhdlnbpafiamejdnhcphjbkeiagm" # ublock origin
          # "njdfdhgcmkocbgbhcioffdbicglldapd" # localcdn
          "hfjbmagddngcpeloejdejnfgbamkjaeg" # vimium c
          "hkgfoiooedgoejojocmhlaklaeopbecg" # picture-in-picture
          "iaiomicjabeggjcfkbimgmglanimpnae" # tab session manager
          "mmcgnaachjapbbchcpjihhgjhpfcnoan" # open new tab after current tab
          "nacjakoppgmdcpemlfnfegmlhipddanj" # pdf.js with vimium
          "bgfofngpplpmpijncjegfdgilpgamhdk" # modern scrollbar
          "opcjanmpjbdbdpnjfjbboacibokblbhl" # mut tab shortcuts
        ];
      };
  };
}
