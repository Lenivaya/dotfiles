{ config, options, lib, pkgs, ... }:

with lib; {

  options.modules.desktop.xmonad = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf config.modules.desktop.xmonad.enable {
    services.xserver.windowManager.xmonad = {
      enable = true;
      haskellPackages = pkgs.unstable.haskellPackages;
      extraPackages = haskellPackages: [
        haskellPackages.xmonad-contrib
        haskellPackages.xmonad-extras
        haskellPackages.xmonad
        haskellPackages.xmobar
        haskellPackages.gloss
      ];
    };

    my.env = {
      XMONAD_CONFIG_DIR = <config/xmonad>;
      XMONAD_CACHE_DIR = "$XDG_CACHE_HOME/xmonad";
      XMONAD_DATA_DIR = "$XDG_DATA_HOME/xmonad";
    };

    my.packages = with pkgs.unstable; [ xmobar ];
    fonts.fonts = with pkgs; [ siji ];

    modules.desktop.term.st.enable = true;
    modules.desktop.term.default = "st";
  };
}
