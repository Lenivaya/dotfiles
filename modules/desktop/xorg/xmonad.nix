# https://github.com/doums/apekey ?
# https://gist.github.com/altercation/fdd2dff789b4aa9287476bf33ba6167c (named actions) ?
{
  config,
  lib,
  pkgs,
  inputs,
  system,
  ...
}:
with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  cfg = config.modules.desktop.xmonad;
in
{
  options.modules.desktop.xmonad.enable = mkBoolOpt false;

  imports = with inputs; xmonad-contrib.nixosModules ++ [ xmonad-contrib.modernise.${system} ];

  config = mkIf cfg.enable {
    modules = {
      services = {
        avizo = enabled;
        skippy-xd = enabled;
      };
      desktop = {
        lockscreen = enabled;
        compositor = enabled;
        term = {
          alacritty = enabled;
          default = mkForce "alacritty";
        };

        apps = {
          dunst = enabled;
          rofi = enabled;
          dmenu = enabled;
        };
      };
    };

    services.xserver.windowManager.xmonad = enabled // {
      # enableContribAndExtras = true;
      extraPackages =
        hpkgs: with hpkgs; [
          xmonad-contrib
          flow
        ];
      flake = enabled;
    };

    services.displayManager.defaultSession = "none+xmonad";

    environment.sessionVariables = {
      XMONAD_CONFIG_DIR = "$XDG_CONFIG_HOME/dotfiles/config/xmonad";
      XMONAD_CACHE_DIR = "$XDG_CACHE_HOME/xmonad";
      XMONAD_DATA_DIR = "$XDG_DATA_HOME/xmonad";
    };

    user.packages = with pkgs; [
      xmonadctl
      xmobar
      trayer # tray
      jq # for weather script
      playerctl # current track script
      pstree # window swallowing
      xdotool # cliclable workspaces
      autorandr # to use with rescreen
      arandr
      xkb-switch # switch kbd layout when needed
      my.boomer # zooming the screen
      xkbmon # showing keyboard layout when changed
      rofi-screenshot # screencasting
    ];
    fonts.packages = with pkgs; [
      siji # some nice icon
      font-awesome # even more nice icons
      weather-icons # for weather script
    ];

    services.autorandr = enabled // {
      hooks = {
        postswitch = {
          change-wallpaper = "source ~/.fehbg";
        };
      };
    };

    systemd.user.services.xmonad-xkbmon = mkGraphicalService {
      description = "XMonad keyboard monitor";
      path = with pkgs; [
        xkbmon
        xmonadctl
      ];
      script = readFile "${configDir}/xmonad/scripts/keyboard-listener";
    };

    env.PATH = [ "$DOTFILES/config/xmonad/scripts/xmobar" ];
  };
}
