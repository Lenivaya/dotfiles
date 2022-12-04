{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.xmonad;
in {
  options.modules.desktop.xmonad = {
    enable = mkBoolOpt false;
    # withKde = mkBoolOpt false;
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
        haskellPackages.flow
      ];
      ghcArgs = ["-O3"];
    };

    services.xserver.displayManager.defaultSession = "none+xmonad";

    environment.sessionVariables = {
      XMONAD_CONFIG_DIR = "$XDG_CONFIG_HOME/dotfiles/config/xmonad";
      XMONAD_CACHE_DIR = "$XDG_CACHE_HOME/xmonad";
      XMONAD_DATA_DIR = "$XDG_DATA_HOME/xmonad";
    };

    user.packages = with pkgs; [
      xmobar
      jq # for weather script
      playerctl # current track script
      pstree # window swallowing
      xdotool # cliclable workspaces
    ];
    fonts.fonts = with pkgs; [
      siji # some nice icons (awfull on hidpi)
      font-awesome # even more nice icons
      weather-icons # for weather script
    ];

    env.PATH = ["$DOTFILES/config/xmonad/scripts/xmobar"];

    modules.desktop.term.alacritty.enable = true;
    modules.desktop.term.default = lib.mkForce "alacritty";
  };
}
