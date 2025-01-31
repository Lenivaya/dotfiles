{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.desktop.term.kitty;
  inherit (config.dotfiles) configDir;
in
{
  options.modules.desktop.term.kitty = {
    enable = mkBoolOpt false;
    # Makes faster.
    # https://wiki.archlinux.org/title/Kitty#Single_instance_mode
    singleInstance = mkBoolOpt true;
    default = mkBoolOpt false;
  };

  config = mkIf cfg.enable (mkMerge [
    {
      user.packages = with pkgs; [ kitty ];

      home.configFile."kitty" = {
        source = "${configDir}/kitty";
        recursive = true;
      };
    }

    (mkIf cfg.default {
      modules.desktop.term.default =
        let
          kitty' = if cfg.singleInstance then "kitty --single-instance" else "kitty";
        in
        mkForce kitty';
    })
  ]);
}
