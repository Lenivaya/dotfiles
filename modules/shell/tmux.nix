{
  config,
  pkgs,
  lib,
  ...
}:
with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  cfg = config.modules.shell.tmux;
in
{
  options.modules.shell.tmux.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    modules.shell = {
      fzf = enabled;
    };

    user.packages = with pkgs; [
      tmux
      (makeDesktopItem {
        name = "tmux";
        desktopName = "Tmux";
        exec = "${config.modules.desktop.term.default} -e tmux";
        categories = [ "System" ];
      })
    ];

    env.TMUX_HOME = "$XDG_CONFIG_HOME/tmux";

    home.configFile = {
      "tmux" = {
        source = "${configDir}/tmux";
        recursive = true;
      };
      "tmux/plugins".text =
        let
          plugins = with pkgs.tmuxPlugins; [
            prefix-highlight
            yank
            sensible
            resurrect
            continuum
            tmux-thumbs
            jump
            fpp
            extrakto
            tmux-fzf
            fzf-tmux-url
            fuzzback
          ];
          loadPlugin = p: "run-shell ${p.rtp}";
        in
        concatLines (map loadPlugin plugins);
    };
  };
}
