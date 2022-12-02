{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.vm.wine;
in {
  options.modules.desktop.vm.wine.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      wineWowPackages.staging
      winetricks
      bottles
      # (winetricks.override {wine = wineWowPackages.staging;})
    ];
  };
}
