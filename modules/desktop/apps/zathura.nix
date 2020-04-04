{ config, options, lib, pkgs, ... }:

with lib; {
  options.modules.desktop.apps.zathura = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf config.modules.desktop.apps.zathura.enable {
    my.packages = with pkgs; [ zathura ];

    my.home.xdg.configFile."zathura" = {
      source = <config/zathura>;
      recursive = true;
    };
  };
}
