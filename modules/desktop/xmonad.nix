{ config, lib, pkgs, ... }:

let unstable = import <unstable> { };
in {
  services.xserver.windowManager.xmonad = {
    enable = true;
    haskellPackages = pkgs.unstable.haskellPackages;
    extraPackages = haskellPackages: [
      haskellPackages.xmonad-contrib
      haskellPackages.xmonad-extras
      haskellPackages.xmonad
      haskellPackages.xmobar
    ];
  };

  my.env = {
    XMONAD_CONFIG_DIR = <config/xmonad>;
    XMONAD_CACHE_DIR = "$XDG_CACHE_HOME/xmonad";
    XMONAD_DATA_DIR = "$XDG_DATA_HOME/xmonad";
  };

  my.packages = with pkgs.unstable; [ xmobar ];
}
