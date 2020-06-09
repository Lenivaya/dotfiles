{ config, lib, pkgs, ... }:
let
  # The developer of tmux chooses not to add XDG support for religious
  # reasons (see tmux/tmux#142). Fortunately, nix makes this easy:
  tmuxPackage = with pkgs;
    writeScriptBin "tmux" ''
      #!${stdenv.shell}
      exec ${tmux}/bin/tmux -f "$TMUX_HOME/config" "$@"
    '';
  tmuxDesktopItem = with pkgs;
    makeDesktopItem {
      name = "tmux";
      desktopName = "Tmux";
      exec = "${config.modules.desktop.term.default} -e tmux";
      categories = "System";
    };
in {
  my = {
    packages = [ (tmuxPackage) (tmuxDesktopItem) ];

    env.TMUX_HOME = "$XDG_CONFIG_HOME/tmux";

    home.xdg.configFile = {
      "tmux" = {
        source = <config/tmux>;
        recursive = true;
      };
      "tmux/plugins".text = ''
        run-shell ${pkgs.tmuxPlugins.copycat}/share/tmux-plugins/copycat/copycat.tmux
        run-shell ${pkgs.tmuxPlugins.yank}/share/tmux-plugins/yank/yank.tmux
        run-shell ${pkgs.tmuxPlugins.resurrect}/share/tmux-plugins/resurrect/resurrect.tmux
        run-shell ${pkgs.tmuxPlugins.continuum}/share/tmux-plugins/continuum/continuum.tmux
        run-shell ${pkgs.tmuxPlugins.sensible}/share/tmux-plugins/sensible/sensible.tmux
      '';
    };
  };

  ## Automatic start
  systemd.user.services.tmux = {
    unitConfig = { Description = "tmux default session"; };
    after = [ "graphical-session-pre.target" ];
    partOf = [ "graphical-session.target" ];
    wantedBy = [ "graphical-session.target" ];

    path = [ (tmuxPackage) pkgs.ripgrep ];
    environment = {
      DISPLAY = "0";
      TMUX_HOME = "/home/${config.my.username}/.config/tmux";
    };

    script = ''
      tmux new-session -d -s "main" -n "main"
    '';
    preStop = ''
      tmux list-sessions | rg -q "main" && tmux kill-session -t main
      ${pkgs.tmuxPlugins.resurrect}/share/tmux-plugins/resurrect/scripts/save.sh
      tmux kill-server
    '';

    serviceConfig = {
      Type = "forking";
      KillMode = "none";
      RestartSec = 2;
    };
  };
}
