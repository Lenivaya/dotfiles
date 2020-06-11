{ config, options, lib, pkgs, ... }:

let
  cfg = config.modules.desktop.browsers.chromium;

  wrapWithFlags = p: f:
    with pkgs;
    writeScriptBin "${p.packageName}" ''
      #!${stdenv.shell}
      exec ${p}/bin/${p.packageName} ${lib.concatStringsSep " " f}
    '';

in with lib; {
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
    my.home.programs.chromium = {
      enable = true;

      package = if cfg.ungoogled then
        (wrapWithFlags pkgs.unstable.ungoogled-chromium cfg.flags)
      else
        (wrapWithFlags pkgs.chromium cfg.flags);

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
      "--ignore-gpu-blacklist"
      "--enable-gpu-rasterization"
      "--enable-native-gpu-memory-buffers"
      "--enable-zero-copy"
    ];

  };
}
