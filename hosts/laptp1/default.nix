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
  ...
}:
with lib;
with my; {
  imports = [
    ../common.nix
    ./hardware-configuration.nix
    ./adguard-home.nix
    ./radarr.nix
    ./books.nix
    ./prowlarr.nix
    ./jellyfin.nix
    ./torrents.nix
    ./nginx.nix
    ./homepage/dashboard.nix
  ];

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
  };

  # zramSwap = {
  #   algorithm = mkForce "lz4";
  #   memoryPercent = mkForce 50;
  # };

  users.groups.media = {};
  user.extraGroups = ["media"];

  # jellyfin and video things
  services.xserver = {videoDrivers = ["radeon"];};

  services.logind.lidSwitchExternalPower = "ignore";
  networking.networkmanager.wifi.macAddress = mkForce "permanent";
  services.irqbalance = enabled;
  powerManagement.cpuFreqGovernor = mkForce "performance";

  # Disabling some things that use memory
  # but are completely unrequired
  nix.settings.auto-optimise-store = mkForce false;
  nix.gc.automatic = mkForce false;
  services.thermald = mkForce disabled;
  services.automatic-timezoned = mkForce disabled;
  location.provider = mkForce "manual";
  security.polkit = mkForce disabled;
  hardware.bluetooth = disabled;
  sound = disabled;

  # Kernel (lts)
  boot.kernelPackages = mkForce pkgs.linuxPackages;

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
    stopMedia = "sudo systemctl stop radarr readarr prowlarr calibre-server calibre-web jellyfin";
    startMedia = "sudo systemctl restart radarr readarr prowlarr calibre-server calibre-web jellyfin";
  };
}
