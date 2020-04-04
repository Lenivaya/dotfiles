{ pkgs, ... }: {
  my = {
    packages = with pkgs; [ nodejs yarn ];

    env.NPM_CONFIG_USERCONFIG = "$XDG_CONFIG_HOME/npm/config";
    env.NPM_CONFIG_CACHE = "$XDG_CACHE_HOME/npm";
    env.NPM_CONFIG_TMP = "$XDG_RUNTIME_DIR/npm";
    env.NPM_CONFIG_PREFIX = "$XDG_CACHE_HOME/npm";
    env.NODE_REPL_HISTORY = "$XDG_CACHE_HOME/node/repl_history";
    env.PATH = [ "$(yarn global bin)" ];

    home.xdg.configFile."npm/config".text = ''
      cache=$XDG_CACHE_HOME/npm
      prefix=$XDG_DATA_HOME/npm
    '';
  };
}
