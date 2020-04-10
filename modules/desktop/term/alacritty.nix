{ config, lib, pkgs, ... }:

with lib; {
  options.modules.desktop.term.alacritty = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf config.modules.desktop.term.alacritty.enable {
    environment.systemPackages = with pkgs; [ alacritty ];
  };

}
