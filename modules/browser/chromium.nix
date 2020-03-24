{ config, lib, pkgs, ... }:

{

  my.programs.chromium = {
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

}
