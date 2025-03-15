{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.desktop.browsers.chromium;
in
{
  options.modules.desktop.browsers.chromium = with types; {
    package = mkOpt package pkgs.google-chrome;
    enable = mkBoolOpt false;
    commandLineArgs = mkOpt (listOf str) [
      # Dark theme
      "--force-dark-mode"

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
      "--enable-gpu-compositing"

      "--smooth-scrolling"
      "--enable-smooth-scrolling"

      "--sharing-desktop-screenshots"

      "--enable-unsafe-webgpu"

      # Fuck this "outdated" shit
      "--simulate-outdated-no-au='Tue, 31 Dec 2099 23:59:59 GMT'"

      "--enable-features=${
        comcat [
          "WebUIDarkMode"
          "ParallelDownloading"
          "BackForwardCache:enable_same_site/true"
          "CopyLinkToText"
          "OverlayScrollbar"
          "TabHoverCardImages"
          "Vulkan"
          "DefaultANGLEVulkan"
          "VulkanFromANGLE"
          "VaapiVideoEncoder"
          "VaapiVideoDecoder"
          "VaapiIgnoreDriverChecks"
          "VaapiVideoDecodeLinuxGL"
          "AcceleratedVideoDecodeLinuxGL"
          "AcceleratedVideoEncoder"
          "CanvasOopRasterization"
          "TouchpadOverscrollHistoryNavigation"
        ]
      }"
    ];
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ ff2mpv-rust ];

    home.programs.chromium = enabled // {
      inherit (cfg) package commandLineArgs;

      extensions = [
        "cjpalhdlnbpafiamejdnhcphjbkeiagm" # ublock origin
        "hfjbmagddngcpeloejdejnfgbamkjaeg" # vimium c
        "hkgfoiooedgoejojocmhlaklaeopbecg" # picture-in-picture
        "iaiomicjabeggjcfkbimgmglanimpnae" # tab session manager
        "aghdiknflpelpkepifoplhodcnfildao" # tab session manager groups
        "mmcgnaachjapbbchcpjihhgjhpfcnoan" # open new tab after current tab
        "nacjakoppgmdcpemlfnfegmlhipddanj" # pdf.js with vimium
        # "bgfofngpplpmpijncjegfdgilpgamhdk" # modern scrollbar
        "opcjanmpjbdbdpnjfjbboacibokblbhl" # mut tab shortcuts
        # "njdfdhgcmkocbgbhcioffdbicglldapd" # localcdn
        "mnjggcdmjocbbbhaepdhchncahnbgone" # sponsorblock

        "hjdoplcnndgiblooccencgcggcoihigg" # didn't read, terms of service
        # "edibdbjcniadpccecjdfdjjppcpchdlm" # I still don't care about cookies

        "hlepfoohegkhhmjieoechaddaejaokhf" # refined github
        "njmimaecgocggclbecipdimilidimlpl" # reddit comment collapser
        "kbmfpngjjgdllneeigpgjifpgocmfgmb" # reddit enhancement suite

        {
          id = "lkbebcjgcmobigpeffafkodonchffocl";
          updateUrl = "https://gitlab.com/magnolia1234/bypass-paywalls-chrome-clean/-/raw/master/updates.xml?ref_type=heads";
        }
      ];
    };
  };
}
