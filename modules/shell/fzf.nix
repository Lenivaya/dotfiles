{
  config,
  pkgs,
  lib,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.shell.fzf;
in
{
  options.modules.shell.fzf.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ fd ];

    home.programs.fzf = enabled // {
      defaultCommand = "fd --type f";
      changeDirWidgetCommand = "fd --type d";
      defaultOptions = [
        "--reverse"
        "--info=inline"
        "--border"
        "--margin=1"
        "--multi"
      ];
    };
  };
}
