{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.sxiv;
in {
  options.modules.desktop.apps.zathura = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    my.packages = with pkgs; [ zathura ];

    home.configFile."zathura" = {
      source = <config/zathura>;
      recursive = true;
    };
  };
}
