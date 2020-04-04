{ config, lib, pkgs, ... }:

{
  my = {
    packages = with pkgs;
      [
        # The developer of tmux chooses not to add XDG support for religious
        # reasons (see tmux/tmux#142). Fortunately, nix makes this easy:
        (writeScriptBin "tmux" ''
          #!${stdenv.shell}
          exec ${tmux}/bin/tmux -f "$TMUX_HOME/config" "$@"
        '')
      ];

    env.TMUX_HOME = "$XDG_CONFIG_HOME/tmux";

    home.xdg.configFile = {
      "tmux" = {
        source = <config/tmux>;
        recursive = true;
      };
      "tmux/plugins".text = ''
        run-shell ${pkgs.tmuxPlugins.copycat}/share/tmux-plugins/copycat/copycat.tmux
        run-shell ${pkgs.tmuxPlugins.yank}/share/tmux-plugins/yank/yank.tmux
        run-shell ${pkgs.tmuxPlugins.continuum}/share/tmux-plugins/continuum/continuum.tmux
        run-shell ${pkgs.tmuxPlugins.resurrect}/share/tmux-plugins/resurrect/resurrect.tmux
        run-shell ${pkgs.tmuxPlugins.sensible}/share/tmux-plugins/sensible/sensible.tmux
      '';
    };
  };
}
