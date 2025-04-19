# Amazing tool to use multiple linux distributions on the same machine (without dealing with VMs)
#
# https://github.com/Aylur/dotfiles/blob/4b66557333bd7199fa858e3275405d831287be37/home-manager/distrobox.nix#L42
# https://github.com/drawbu/dotfiles/blob/251220245f848075f32b11903523006fa925ff1c/home/clement/distrobox.nix#L8

{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.programs.distrobox;
in
{
  options.modules.programs.distrobox = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    # It's better to use distrobox with podman
    virtualisation.podman = enabled // {
      defaultNetwork.settings.dns_enabled = true;
      dockerCompat = false; # Just have both installed at the same time
      enableNvidia = any (v: v == "nvidia") config.services.xserver.videoDrivers;
    };

    home.configFile."distrobox/distrobox.conf".text = ''
      container_manager="podman"
    '';
    environment.sessionVariables = {
      DBX_CONTAINER_MANAGER = "podman";
    };

    environment.systemPackages = with pkgs; [
      distrobox
      distrobox-tui

      podman
      podman-compose
      podman-tui
    ];
    # ++ optional config.this.isHeadful boxbuddy;

    user.extraGroups = [ "podman" ];

    nixpkgs.overlays = [
      (_final: prev: {
        distrobox = prev.distrobox_git;
      })
    ];
  };
}
