# https://github.com/doums/apekey ?
# https://gist.github.com/altercation/fdd2dff789b4aa9287476bf33ba6167c (named actions) ?
# https://github.com/prikhi/xmonad-config
{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  cfg = config.modules.desktop.xmonad;

  flakeCfg = config.services.xserver.windowManager.xmonad;
  hoverlay = _final: _prev: hself: _hsuper: {
    xmonad-extras = hself.callCabal2nix "xmonad-extras" inputs.xmonad-extras { };
  };
  comp = {
    inherit (flakeCfg.flake) prefix compiler;
  };
  xmonadExtrasOverlay = inputs.xmonad.lib.fromHOL hoverlay comp;
in
{
  options.modules.desktop.xmonad.enable = mkBoolOpt false;

  imports = with inputs; xmonad-contrib.nixosModules;
  # ++ [ xmonad-contrib.modernise.${system} ];

  config = mkIf cfg.enable {
    nixpkgs.overlays = [ xmonadExtrasOverlay ];

    modules = {
      services = {
        avizo = enabled;
        skippy-xd = enabled;
        greenclip = enabled;
      };
      desktop = {
        lockscreen = enabled;
        compositor = enabled;
        term = mkDefault {
          kitty = enabled;
          default = mkForce "kitty";
        };

        apps = {
          dunst = enabled;
          rofi = enabled;
          dmenu = enabled;
        };
      };
    };

    services.xserver.windowManager.xmonad = enabled // {
      flake = enabled;
      enableContribAndExtras = true;
      extraPackages = hpkgs: with hpkgs; [ flow ];
    };

    services.displayManager.defaultSession = "none+xmonad";

    environment.sessionVariables = {
      XMONAD_CONFIG_DIR = "${envVar "XDG_CONFIG_HOME"}/dotfiles/config/xmonad";
      XMONAD_CACHE_DIR = "${envVar "XDG_CACHE_HOME"}/xmonad";
      XMONAD_DATA_DIR = "${envVar "XDG_DATA_HOME"}/xmonad";
    };

    environment.systemPackages = with pkgs; [
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
          restart-xmonad = "xmonad --restart";
        };
      };
    };

    systemd.user.services.xmonad-xkbmon = mkGraphicalService {
      description = "XMonad keyboard monitor";
      path = with pkgs; [
        bash
        xkbmon
        xmonadctl
      ];
      script = "${configDir}/xmonad/scripts/keyboard-listener";
    };

    systemd.user.services.trayer = mkGraphicalService {
      enable = mkForce true;
      description = "Trayer, for xmonad";
      path = with pkgs; [ trayer ];
      script = spaceConcat [
        "trayer"
        "-l"
        "--SetDockType true --SetPartialStrut false"
        "--edge top --align right"
        "--widthtype request --expand true"
        "--monitor primary"
        "--tint 0x0B0806"
        "--transparent true --alpha 10"
        "--distancefrom top,right --distance 22,27"
        "--height 25 --iconspacing 3 --padding 1 --margin 1"
      ];
    };

    env.PATH = [ "$DOTFILES/config/xmonad/scripts/xmobar" ];
  };
}
