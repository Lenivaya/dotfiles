{ config, options, lib, pkgs, ... }:

with lib;
let cfg = config.modules.desktop.xmonad;
in {

  options.modules.desktop.xmonad = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
    withKde = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    modules.desktop.WM.enable = true;

    services.xserver.windowManager.xmonad = {
      enable = true;
      haskellPackages = pkgs.unstable.haskellPackages;
      extraPackages = haskellPackages: [
        haskellPackages.xmonad-contrib
        haskellPackages.xmonad-extras
        haskellPackages.xmonad
        haskellPackages.gloss
      ];
    };

    services.xserver = { displayManager.defaultSession = "none+xmonad"; };

    my.env = {
      XMONAD_CONFIG_DIR = <config/xmonad>;
      XMONAD_CACHE_DIR = "$XDG_CACHE_HOME/xmonad";
      XMONAD_DATA_DIR = "$XDG_DATA_HOME/xmonad";
    };

    my.packages = with pkgs.unstable; [
      xmobar
      jq # for weather script
    ];
    fonts.fonts = with pkgs; [
      siji # some nice icons
      weather-icons # for weather script
    ];

    my.env.PATH = [ <config/xmonad/scripts/xmobar> "$PATH" ];

    modules.desktop.term.st.enable = true;
    modules.desktop.term.default = "st";

  };

}
