# https://github.com/doums/apekey ?
{
  config,
  options,
  lib,
  pkgs,
  inputs,
  system,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.xmonad;
in {
  options.modules.desktop.xmonad.enable = mkBoolOpt false;

  imports = with inputs;
    xmonad-contrib.nixosModules
    ++ [xmonad-contrib.modernise.${system}];

  config = mkIf cfg.enable {
    services.xserver.windowManager.xmonad =
      enabled
      // {
        enableContribAndExtras = true;
        extraPackages = hpkgs:
          with hpkgs; [
            flow
          ];
        ghcArgs = [
          "-O3"
          # Compile with LLVM backend
          # "-fllvm -optlo-O3"
        ];
        flake = enabled;
      };

    services.xserver.displayManager.defaultSession = "none+xmonad";

    environment.sessionVariables = {
      XMONAD_CONFIG_DIR = "$XDG_CONFIG_HOME/dotfiles/config/xmonad";
      XMONAD_CACHE_DIR = "$XDG_CACHE_HOME/xmonad";
      XMONAD_DATA_DIR = "$XDG_DATA_HOME/xmonad";
    };

    user.packages = with pkgs; [
      xmobar
      trayer # tray
      jq # for weather script
      playerctl # current track script
      pstree # window swallowing
      xdotool # cliclable workspaces
      autorandr # to use with rescreen
      xkb-switch # switch kbd layout when needed
      my.boomer # zooming the screen
    ];
    fonts.fonts = with pkgs; [
      siji # some nice icons (awfull on hidpi)
      font-awesome # even more nice icons
      weather-icons # for weather script
    ];

    env.PATH = ["$DOTFILES/config/xmonad/scripts/xmobar"];

    modules.desktop.term.alacritty = enabled;
    modules.desktop.term.default = mkForce "alacritty";
  };
}
