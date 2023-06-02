{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.shell.git;
in {
  options.modules.shell.git.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    home.programs.git = {
      enable = true;
      delta.enable = true;

      aliases = {
        graph = "log --graph --decorate --oneline";
        map = "!git graph --all";
        watch = "!watch -ct 'git -c color.status=always status -s && echo && git map --color'";
      };
    };

    user.packages = with pkgs; [
      gitAndTools.gh
      gitAndTools.git-open
      # gitAndTools.diff-so-fancy
      (mkIf config.modules.shell.gnupg.enable gitAndTools.git-crypt)
      lazygit
    ];
  };
}
