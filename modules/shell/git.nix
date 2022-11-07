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
