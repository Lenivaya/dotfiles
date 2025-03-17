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

  config = mkIf cfg.enable {
    nixpkgs.overlays = [ xmonadExtrasOverlay ];

    modules = {
      services = {
        avizo = enabled;
        skippy-xd = enabled;
        clipcat = enabled;
      };
      shell = {
        yazi = enabled;
      };
      desktop = {
        lockscreen = enabled;
        # compositor = enabled;
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
      extraPackages =
        hpkgs: with hpkgs; [
          flow
          fuzzyfind
        ];
      enableConfiguredRecompile = true;
      ghcArgs = [
        "-hidir /tmp" # place interface files in /tmp, otherwise ghc tries to write them to the nix store
        "-odir /tmp" # place object files in /tmp, otherwise ghc tries to write them to the nix store
        "-O3"
        "-funfolding-use-threshold=16"
        "-fexcess-precision"
        "-optc-O3"
        "-optc-march=native"
        "-optc-mtune=native"
        "-optc-ffast-math"

        "-fdicts-cheap"
        "-fspecialise-aggressively"
        "-fblock-layout-weightless"
        "-feager-blackholing"
        "-fexpose-all-unfoldings"
        "-fregs-iterative"
        "-fspec-constr-keen"
        "-fstatic-argument-transformation"
        "-funbox-strict-fields"
        "-flate-dmd-anal"

        "-flto"
        "-optc-flto"
        "-optl-flto"
      ];
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

    services.autorandr = enabled // {
      hooks = {
        postswitch = {
          change-wallpaper = "source ~/.fehbg";
          restart-xmonad = "xmonad --restart";
          restart-avizo = "systemctl --user restart avizo";
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
        "--distancefrom top,right --distance 13,13"
        "--height 25 --iconspacing 3 --padding 1 --margin 1"
      ];
    };

    env.PATH = [ "$DOTFILES/config/xmonad/scripts/xmobar" ];
  };
}
