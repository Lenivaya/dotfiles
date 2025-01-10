{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.desktop.vm.winapps;
in
{
  options.modules.desktop.vm.winapps.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    modules = {
      dev.docker = enabled;
    };

    environment.systemPackages = with pkgs; [
      dialog
      inputs.winapps.packages.${pkgs.system}.winapps
      inputs.winapps.packages.${pkgs.system}.winapps-launcher
    ];
  };
}
