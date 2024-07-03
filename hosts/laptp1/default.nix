# laptp1 -- old laptop used as a server
#
# nixos-rebuild \
#     --flake "$DOTFILES"/.#laptp1 \
#     --show-trace --use-remote-sudo \
#     --impure --fast --no-build-nix \
#     --target-host leniviy@192.168.0.99 \
#     switch
#
{
  pkgs,
  lib,
  inputs,
  ...
}:
with lib;
with my; {
  imports =
    [
      ../common.nix
      ./hardware-configuration.nix
      ./modules/default.nix
      ./modules/your-spotify.nix
    ]
    ++ (with inputs.srvos; [
      nixosModules.server
      nixosModules.mixins-nginx
    ])
    ++ [inputs.chaotic.nixosModules.default];

  this.isHeadless = true;

  modules = {
    shell = {
      gnupg = enabled;
      tmux = enabled;
      git = enabled;
      zsh = enabled;
    };

    services = {
      ssh = enabled;
    };

    hardware = {
      cpu.intel = enabled;
      fs = enabled;
      # zram = enabled;
    };

    fast-networking = enabled;
  };

  # overriding after srvos
  networking.hostName = mkForce "laptp1";

  # zramSwap = {
  #   algorithm = mkForce "lz4";
  #   memoryPercent = mkForce 50;
  # };

  users.groups.media = {};
  user.extraGroups = ["media"];

  # jellyfin and video things
  services.xserver = {videoDrivers = ["radeon"];};

  services.logind.lidSwitchExternalPower = "ignore";
  networking.networkmanager.wifi.powersave = mkForce false;
  networking.networkmanager.wifi.macAddress = mkForce "permanent";
  # services.irqbalance = enabled;
  powerManagement.cpuFreqGovernor = mkForce "performance";
  fileSystems."/".options = ["noatime" "nodiratime"];

  # Disabling some things that use memory
  # but are completely unrequired
  nix.settings.auto-optimise-store = mkForce false;
  nix.gc.automatic = mkForce false;
  services.thermald = mkForce disabled;
  services.automatic-timezoned = mkForce disabled;
  location.provider = mkForce "manual";
  security.polkit = mkForce disabled;
  hardware.bluetooth = mkForce disabled;
  sound = mkForce disabled;
  fonts.fontconfig = mkForce disabled;
  boot.tmp.useTmpfs = mkForce false;

  boot.kernelPackages = let
    kernel' = pkgs.linuxPackages_cachyos-server;
  in
    mkForce kernel';

  services.bpftune = enabled;

  boot.kernelParams = [
    # HACK Disables fixes for spectre, meltdown, L1TF and a number of CPU
    #      vulnerabilities. Don't copy this blindly! And especially not for
    #      mission critical or server/headless builds exposed to the world.
    "mitigations=off"

    # https://wiki.archlinux.org/title/improving_performance#Watchdogs
    "nowatchdog"
    "kernel.nmi_watchdog=0"
  ];

  environment.shellAliases = {
    freemem = "sync && echo 3 | sudo tee /proc/sys/vm/drop_caches";
    stopMedia = "sudo systemctl stop radarr sonarr readarr prowlarr calibre-server calibre-web jellyfin";
    startMedia = "sudo systemctl restart radarr sonarr readarr prowlarr calibre-server calibre-web jellyfin";
  };

  nixpkgs.overlays = [
    (_final: prev: {
      inherit
        (pkgs.unstable)
        bpftune
        jellyfin
        jellyfin-web
        ;
    })
  ];
}
