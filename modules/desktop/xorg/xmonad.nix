{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.xmonad;
in {

  options.modules.desktop.xmonad = {
    enable = mkBoolOpt false;
    withKde = mkBoolOpt false;
  };

  config = mkIf cfg.enable {

    services.xserver.windowManager.xmonad = {
      enable = true;
      # haskellPackages = pkgs.unstable.haskellPackages;
      extraPackages = haskellPackages: [
        haskellPackages.xmonad-contrib
        haskellPackages.xmonad-extras
        haskellPackages.xmonad
        haskellPackages.gloss
      ];
      ghcArgs = [ "-O2" ];
    };

    services.xserver = { displayManager.defaultSession = "none+xmonad"; };

    env = {
      XMONAD_CONFIG_DIR = "$XDG_CONFIG_HOME/dotfiles/config/xmonad";
      XMONAD_CACHE_DIR = "$XDG_CACHE_HOME/xmonad";
      XMONAD_DATA_DIR = "$XDG_DATA_HOME/xmonad";
    };

    user.packages = with pkgs; [
      xmobar
      jq # for weather script
    ];
    fonts.fonts = with pkgs; [
      siji # some nice icons
      weather-icons # for weather script
    ];

    env.PATH = [ "${configDir}/xmonad/scripts/xmobar" ];

    modules.desktop.term.st.enable = true;
    modules.desktop.term.default = "st";

  };

}
