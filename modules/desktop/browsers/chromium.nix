{ config, options, lib, pkgs, ... }:

with lib; {
  options.modules.desktop.browsers.chromium = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf config.modules.desktop.browsers.chromium.enable {

    my.home.programs.chromium = {
      enable = true;
      package = pkgs.google-chrome;
      extensions = [
        "cjpalhdlnbpafiamejdnhcphjbkeiagm" # ublock origin
        "hfjbmagddngcpeloejdejnfgbamkjaeg" # vimium c
        "cglpcedifkgalfdklahhcchnjepcckfn" # newtab adapter
        # "klbibkeccnjlkjkiokjodocebajanakg" # the great suspender
        "hkgfoiooedgoejojocmhlaklaeopbecg" # picture-in-picture
      ];
    };
  };

}
