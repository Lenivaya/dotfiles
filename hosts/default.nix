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
    efi.canTouchEfiVariables = true;
    systemd-boot.configurationLimit = 10;
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

  networking.firewall.enable = true;
  # Use the latest kernel
  boot.kernelPackages = pkgs.linuxPackages_latest;

  ## System security tweaks
  boot.tmpOnTmpfs = true;
  security.hideProcessInformation = true;
  security.protectKernelImage = true;
  # Fix a security hole in place for backwards compatibility. See desc in
  # nixpkgs/nixos/modules/system/boot/loader/systemd-boot/systemd-boot.nix
  boot.loader.systemd-boot.editor = false;

  # Change me later!
  user.initialPassword = "nix";
  users.users.root.initialPassword = "nix";
}
