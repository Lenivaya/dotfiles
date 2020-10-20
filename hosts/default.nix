# hosts/default.nix
#
# Loads config common to all hosts.

{ config, lib, pkgs, ... }:

with lib;
with lib.my; {
  ## Some reasonable, global defaults
  # This is here to appease 'nix flake check' for generic hosts with no
  # hardware-configuration.nix or fileSystem config.
  fileSystems."/".device = "/dev/disk/by-label/nixos";

  boot.loader = {
    timeout = 1;
    efi.canTouchEfiVariables = true;
    systemd-boot = {
      enable = true;
      configurationLimit = 10;
      # Fix a security hole in place for backwards compatibility. See desc in
      # nixpkgs/nixos/modules/system/boot/loader/systemd-boot/systemd-boot.nix
      editor = false;
    };
  };

  # Just the bear necessities...
  environment.systemPackages = with pkgs; [
    unstable.cached-nix-shell
    coreutils
    git
    vim
    wget
    gnumake
  ];

  networking = {
    networkmanager.enable = true;
    firewall.enable = true;
  };
  # Use the latest kernel
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Auto-mount
  programs = {
    gnome-disks.enable = true;
    udevil.enable = true;
  };

  ## System security tweaks
  boot.tmpOnTmpfs = true;
  security.hideProcessInformation = true;
  security.protectKernelImage = true;

  # hardware related stuff
  hardware.enableRedistributableFirmware = true;
  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
    extraPackages = with pkgs; [
      vaapiIntel
      vaapiVdpau
      libvdpau-va-gl
      intel-media-driver
    ];
  };

  # Change me later!
  user.initialPassword = "nix";
  users.users.root.initialPassword = "nix";
}
