# default.nix --- my dotfile bootstrapper

device: username:
{ pkgs, options, lib, config, ... }: {
  networking.hostName = lib.mkDefault device;

  imports = [ ./modules "${./hosts}/${device}" ];

  ### NixOS
  nix = {
    autoOptimiseStore = true;
    gc = {
      automatic = true;
      dates = "23:00";
      options = "--delete-older-than 30d";
    };
  };

  nix.nixPath = options.nix.nixPath.default ++ [
    # So we can use absolute import paths
    "bin=/etc/dotfiles/bin"
    "config=/etc/dotfiles/config"
  ];

  # Add custom packages & unstable channel, so they can be accessed via pkgs.*
  nixpkgs.overlays = import ./packages;
  nixpkgs.config.allowUnfree = true; # forgive me Stallman senpai

  environment.systemPackages = with pkgs; [
    # Just the bear necessities~
    coreutils
    git
    killall
    unzip
    vim
    wget
    sshfs

    gnumake # for makefile
    pkgs.my.cached-nix-shell # for instant nix-shell scripts
  ];
  environment.shellAliases = {
    nix-env = "NIXPKGS_ALLOW_UNFREE=1 nix-env";
    nix-shell = ''
      NIX_PATH="nixpkgs-overlays=/etc/dotfiles/packages/default.nix:$NIX_PATH" nix-shell'';
    nsh = "nix-shell";
    nen = "nix-env";
    dots = "make -C ~/.dotfiles";
  };

  system.autoUpgrade = {
    enable = true;
    dates = "weekly";
    flags = [
      "-I"
      "config=/etc/dotfiles/config"
      "-I"
      "modules=/etc/dotfiles/modules"
      "-I"
      "bin=/etc/dotfiles/bin"
    ];
  };

  my.username = username;
  my.user = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" "networkmanager" "adbusers" "docker" ];
    shell = pkgs.zsh;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.09"; # Did you read the comment?
}
