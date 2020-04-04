{ config, options, lib, pkgs, ... }:

with lib; {
  options.modules.desktop.apps.dunst = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf config.modules.desktop.apps.dunst.enable {
    my.packages = with pkgs; [ dunst libnotify ];

    my.home.xdg.configFile."dunst" = {
      source = <config/dunst>;
      recursive = true;
    };
  };
}
