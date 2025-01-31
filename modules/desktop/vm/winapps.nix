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
    # modules = {
    #   dev.docker = enabled;
    # };

    # It's better to use winapps with podman
    virtualisation.podman = enabled // {
      defaultNetwork.settings.dns_enabled = true;
      dockerCompat = false; # Just have both installed at the same time
      enableNvidia = any (v: v == "nvidia") config.services.xserver.videoDrivers;
    };

    environment.systemPackages = with pkgs; [
      dialog
      inputs.winapps.packages.${pkgs.system}.winapps
      inputs.winapps.packages.${pkgs.system}.winapps-launcher
    ];
  };
}
