{
  lib,
  pkgs,
  config,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.shell.yazi;
in
{
  options.modules.shell.yazi.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    home.programs.yazi = enabled // {
      settings = {
        manager = {
          layout = [
            0
            3
            5
          ];
          sort_dir_first = true;
          show_symlink = true;
        };

        opener = {
          zathura = [
            {
              run = ''zathura "$@"'';
              for = "unix";
              desc = "Open with zathura";
            }
          ];
        };

        open = {
          append_rules = [
            {
              name = "*.pdf";
              use = [
                "zathura"
                "papers"
              ];
            }
            {
              name = "*.epub";
              use = [ "foliate" ];
            }
          ];
        };
      };
    };

    user.packages = with pkgs; [
      exiftool # general file info
    ];
  };
}
