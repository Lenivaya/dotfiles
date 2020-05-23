{ config, options, lib, pkgs, ... }:

let cfg = config.modules.desktop.browsers.chromium;
in with lib; {
  options.modules.desktop.browsers.chromium = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
    ungoogled = mkOptions {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    my.home.programs.chromium = {
      enable = true;

      package = if cfg.ungoogled then
        pkgs.unstable.ungoogled-chromium
      else
        pkgs.chromium;

      extensions = [
        "cjpalhdlnbpafiamejdnhcphjbkeiagm" # ublock origin
        "pkehgijcmpdhfbdbbnkijodmdjhbjlgp" # privacy badger
        "ldpochfccmkkmhdbclfhpagapcfdljkj" # decentraleyes
        "hfjbmagddngcpeloejdejnfgbamkjaeg" # vimium c
        "cglpcedifkgalfdklahhcchnjepcckfn" # newtab adapter
        # "klbibkeccnjlkjkiokjodocebajanakg" # the great suspender
        "hkgfoiooedgoejojocmhlaklaeopbecg" # picture-in-picture
      ];
    };

  };
}
