{
  inputs,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my;
{
  imports = [
    inputs.home-manager.nixosModules.home-manager
    inputs.programsdb.nixosModules.programs-sqlite
    inputs.chaotic.nixosModules.default
  ] ++ (with inputs.srvos; [ nixosModules.common ]) ++ (mapModulesRec' (toString ./modules) import);

  # Common config for all nixos machines; and to ensure the flake operates
  # soundly
  environment.variables.DOTFILES = config.dotfiles.dir';
  environment.variables.DOTFILES_BIN = config.dotfiles.binDir;

  # Configure nix and nixpkgs
  environment.variables.NIXPKGS_ALLOW_UNFREE = "1";
  home-manager.users.${config.user.name}.nixpkgs = {
    config = {
      allowUnfree = true;
    };
    inherit (config.nixpkgs) overlays;
  };
  nix =
    let
      filteredInputs = filterAttrs (n: _: n != "self") inputs;
      nixPathInputs = mapAttrsToList (n: v: "${n}=${v}") filteredInputs;
      registryInputs = mapAttrs (_: v: { flake = v; }) filteredInputs;
    in
    {
      extraOptions = "experimental-features = nix-command flakes";
      nixPath = nixPathInputs ++ [
        "nixpkgs-overlays=${config.dotfiles.dir}/overlays"
        "dotfiles=${config.dotfiles.dir}"
      ];
      settings = {
        accept-flake-config = true;
        # use-cgroups = true;
        connect-timeout = 5; # bail early on missing cache hits

        experimental-features =
          let
            features = spaceConcat [
              "flakes"
              "nix-command"
              "repl-flake"
              "big-parallel"
              # "cgroups"
            ];
          in
          mkDefault features;

        warn-dirty = false;

        substituters = [
          "https://nix-community.cachix.org"
          "https://nixpkgs-unfree.cachix.org/"
          "https://cuda-maintainers.cachix.org"
          "https://nixpkgs-unfree.cachix.org" # unfree-package cache
          "https://numtide.cachix.org" # another unfree package cache
          "https://pre-commit-hooks.cachix.org"
          "https://viperml.cachix.org/"
          "https://cache.lix.systems"
        ];
        trusted-public-keys = [
          "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
          "nixpkgs-unfree.cachix.org-1:hqvoInulhbV4nJ9yJOEr+4wxhDV4xq2d1DK7S6Nj6rs="
          "cuda-maintainers.cachix.org-1:0dq3bujKpuEPMCX6U4WylrUDZ9JyUG0VpVZa7CNfq5E="
          "nixpkgs-unfree.cachix.org-1:hqvoInulhbV4nJ9yJOEr+4wxhDV4xq2d1DK7S6Nj6rs="
          "numtide.cachix.org-1:2ps1kLBUWjxIneOy1Ik6cQjb41X0iXVXeHigGmycPPE="
          "pre-commit-hooks.cachix.org-1:Pkk3Panw5AW24TOv6kz3PvLhlH8puAsJTBbOPmBo7Rc="
          "viperml.cachix.org-1:qZhKBMTfmcLL+OG6fj/hzsMEedgKvZVFRRAhq7j8Vh8="
          "cache.lix.systems:aBnZUw8zA7H35Cz2RyKFVs3H4PlGTLawyY5KRbvJR8o="
        ];
        auto-optimise-store = true;
      };
      registry = registryInputs // {
        dotfiles.flake = inputs.self;
      };
      gc = {
        automatic = true;
        dates = "monthly";
        options = "--delete-older-than 30d";
      };
    };
  system.configurationRevision = with inputs; mkIf (self ? rev) self.rev;
  system.stateVersion = "24.11";

  ## Some reasonable, global defaults
  # This is here to appease 'nix flake check' for generic hosts with no
  # hardware-configuration.nix or fileSystem config.
  fileSystems."/".device = mkDefault "/dev/disk/by-label/nixos";

  # Use the latest kernel
  boot.kernelPackages = mkDefault pkgs.linuxPackages_latest;

  boot.initrd.compressor = getExe' pkgs.zstd "zstd";
  boot.loader = {
    efi.canTouchEfiVariables = mkDefault true;
    systemd-boot.configurationLimit = 10;
    systemd-boot.enable = mkDefault true;
    systemd-boot.graceful = mkDefault true;
  };

  # Just the bear necessities...
  environment.systemPackages = with pkgs; [
    cached-nix-shell
    coreutils
    git
    vim
    wget
    gnumake
    unzip

    # I want some scripts to execute with
    # better speed than I have with bash
    # https://unix.stackexchange.com/questions/148035/is-dash-or-some-other-shell-faster-than-bash
    # https://unix.stackexchange.com/questions/397656/what-is-the-fastest-way-to-run-a-script
    # dash
    # ksh
  ];
}
