# xdg.nix
#
# Set up and enforce XDG compliance. Other modules will take care of their own,
# but this takes care of the general case.
{ config, ... }:
{
  ### A tidy $HOME is a tidy mind
  home-manager.users.${config.user.name}.xdg.enable = true;

  environment =
    let
      xdgConventions = {
        # These are the defaults, and xdg.enable does set them, but due to load
        # order, they're not set before environment.variables are set, which could
        # cause race conditions.
        XDG_CACHE_HOME = "$HOME/.cache";
        XDG_CONFIG_HOME = "$HOME/.config";
        XDG_DATA_HOME = "$HOME/.local/share";
        XDG_BIN_HOME = "$HOME/.local/bin";
      };
    in
    {
      sessionVariables = { } // xdgConventions;
      variables = {
        # Conform more programs to XDG conventions. The rest are handled by their
        # respective modules.
        __GL_SHADER_DISK_CACHE_PATH = "$XDG_CACHE_HOME/nv";
        ASPELL_CONF = ''
          per-conf $XDG_CONFIG_HOME/aspell/aspell.conf;
          personal $XDG_CONFIG_HOME/aspell/en_US.pws;
          repl $XDG_CONFIG_HOME/aspell/en.prepl;
        '';
        CUDA_CACHE_PATH = "$XDG_CACHE_HOME/nv";
        HISTFILE = "$XDG_DATA_HOME/bash/history";
        INPUTRC = "$XDG_CONFIG_HOME/readline/inputrc";
        LESSHISTFILE = "$XDG_CACHE_HOME/lesshst";
        WGETRC = "$XDG_CONFIG_HOME/wgetrc";
        ANDROID_HOME = "$XDG_DATA_HOME/android";
        GRIPHOME = "$XDG_CONFIG_HOME/grip";
        PARALLEL_HOME = "$XDG_CONFIG_HOME/parallel";
      } // xdgConventions;
    };
}
