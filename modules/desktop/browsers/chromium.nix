{ config, options, lib, pkgs, home-manager, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.browsers.chromium;

  wrapWithFlags = p: f:
    with pkgs;
    writeScriptBin "${p.packageName}" ''
      #!${stdenv.shell}
      exec ${p}/bin/${p.packageName} ${lib.concatStringsSep " " f}
    '';

in {
  options.modules.desktop.browsers.chromium = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
    ungoogled = mkOption {
      type = types.bool;
      default = false;
    };
    flags = mkOption {
      type = types.listOf types.str;
      default = [ ];
    };
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs;
      [
        (makeDesktopItem {
          name = "chromium";
          desktopName = "Chromium";
          genericName = "Chromium browser";
          icon = "chromium";
          exec = "chromium";
          categories = "Network";
        })
      ];

    home.programs.chromium = {
      enable = true;

      package = with pkgs;
        (wrapWithFlags
          ((if cfg.ungoogled then ungoogled-chromium else chromium)) cfg.flags);

      extensions = [
        "cjpalhdlnbpafiamejdnhcphjbkeiagm" # ublock origin
        "pkehgijcmpdhfbdbbnkijodmdjhbjlgp" # privacy badger
        "ldpochfccmkkmhdbclfhpagapcfdljkj" # decentraleyes
        "hfjbmagddngcpeloejdejnfgbamkjaeg" # vimium c
        "hkgfoiooedgoejojocmhlaklaeopbecg" # picture-in-picture
        "iaiomicjabeggjcfkbimgmglanimpnae" # tab sesssion manager
        "mmcgnaachjapbbchcpjihhgjhpfcnoan" # open new tab after current tab
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
      "--enable-features=VaapiVideoDecoder"
    ];

  };
}
