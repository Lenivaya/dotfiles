{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.dunst;
in {
  options.modules.desktop.apps.dunst = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ dunst libnotify ];

    home.configFile."dunst" = {
      source = <config/dunst>;
      recursive = true;
    };
  };
}
