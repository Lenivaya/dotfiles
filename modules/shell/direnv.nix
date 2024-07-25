{ config, lib, ... }:
with lib;
with lib.my;
let
  cfg = config.modules.shell.direnv;
in
{
  options.modules.shell.direnv.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    programs.direnv = enabled // {
      nix-direnv = enabled;
      silent = true;
      loadInNixShell = true;
    };

    modules.shell.zsh.rcInit = ''eval "$(direnv hook zsh)"'';

    services.lorri = enabled;
  };
}
