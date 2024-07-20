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
in {
  options.modules.desktop.browsers.chromium = with types; {
    package = mkOpt package pkgs.google-chrome;
    enable = mkBoolOpt false;
    commandLineArgs = mkOpt (listOf string) [
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
      "--enable-gpu-compositing"
      "--enable-unsafe-webgpu"

      "--smooth-scrolling"
      "--enable-smooth-scrolling"

      "--sharing-desktop-screenshots"

      # Fuck this "outdated" shit
      "--simulate-outdated-no-au='Tue, 31 Dec 2099 23:59:59 GMT'"

      "--enable-features=${
        comcat [
          "BackForwardCache:enable_same_site/true"
          "CopyLinkToText"
          "OverlayScrollbar"
          "TabHoverCardImages"
          # "Vulkan"
          # "DefaultANGLEVulkan"
          # "VulkanFromANGLE"
          # "RawDraw"
          "VaapiVideoEncoder"
          "VaapiVideoDecoder"
          # "VaapiIgnoreDriverChecks"
          # "VaapiVideoDecodeLinuxGL"
          "CanvasOopRasterization"
          "TouchpadOverscrollHistoryNavigation"
        ]
      }"
    ];
  };

  config = mkIf cfg.enable {
    home.programs.chromium =
      enabled
      // {
        inherit
          (cfg)
          package
          ;

        commandLineArgs =
          cfg.commandLineArgs
          ++ optionals config.modules.desktop.isWayland [
            # Wayland

            # Disabled because hardware acceleration doesn't work
            # when disabling --use-gl=egl, it's not gonna show any emoji
            # and it's gonna be slow as hell
            # "--use-gl=egl"

            "--ozone-platform=wayland"
            "--enable-features=UseOzonePlatform"
          ];

        extensions = [
          "cjpalhdlnbpafiamejdnhcphjbkeiagm" # ublock origin
          "hfjbmagddngcpeloejdejnfgbamkjaeg" # vimium c
          "hkgfoiooedgoejojocmhlaklaeopbecg" # picture-in-picture
          "iaiomicjabeggjcfkbimgmglanimpnae" # tab session manager
          "aghdiknflpelpkepifoplhodcnfildao" # tab session manager groups
          "mmcgnaachjapbbchcpjihhgjhpfcnoan" # open new tab after current tab
          "nacjakoppgmdcpemlfnfegmlhipddanj" # pdf.js with vimium
          "bgfofngpplpmpijncjegfdgilpgamhdk" # modern scrollbar
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
