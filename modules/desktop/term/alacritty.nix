{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.term.alcritty;
in {
  options.modules.desktop.term.alacritty = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    packages = with pkgs.unstable; [ alacritty ];

    home.configFile."alacritty" = {
      source = <config/alacritty>;
      recursive = true;
    };
  };
}
