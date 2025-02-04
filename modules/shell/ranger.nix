{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.shell.ranger;
  inherit (config.dotfiles) configDir;
in
{
  options.modules.shell.ranger.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      ranger

      ueberzugpp
      poppler_utils
      zoxide
    ];

    # Maybe some time
    # home.programs.pistol =
    #   enabled
    #   // {
    #     associations = [];
    #   };

    home.configFile."ranger" = {
      source = "${configDir}/ranger";
      recursive = true;
    };
  };
}
