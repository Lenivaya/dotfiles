# node.nix --- https://nodejs.org/en/
{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.dev.node;
in
{
  options.modules.dev.node = with types; {
    enable = mkBoolOpt false;
    package = mkOpt package pkgs.nodejs;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      cfg.package
      corepack
      node-gyp # https://github.com/kelektiv/node.bcrypt.js/issues/800
      fnm
    ];

    env.NPM_CONFIG_USERCONFIG = "$XDG_CONFIG_HOME/npm/config";
    env.NPM_CONFIG_CACHE = "$XDG_CACHE_HOME/npm";
    env.NPM_CONFIG_TMP = "$XDG_RUNTIME_DIR/npm";
    env.NPM_CONFIG_PREFIX = "$XDG_CACHE_HOME/npm";
    env.NODE_REPL_HISTORY = "$XDG_CACHE_HOME/node/repl_history";
    env.PATH = [ "$(npm config get prefix)/bin" ];

    # Run locally installed bin-script, e.g. n coffee file.coffee
    environment.shellAliases = {
      n = ''PATH="$(npm config get prefix)/bin:$PATH"'';
    };

    home.configFile."npm/config".text = ''
      cache=$XDG_CACHE_HOME/npm
      prefix=$XDG_DATA_HOME/npm
    '';
  };
}
