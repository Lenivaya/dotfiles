{
  description = "A grossly incandescent nixos config.";

  inputs = {
    # Core dependencies
    # nixpkgs.url = "nixpkgs/nixos-unstable"; # primary nixpkgs
    nixpkgs.url = "nixpkgs/nixos-22.11"; # primary nixpkgs
    nixpkgs-unstable.url = "nixpkgs/nixpkgs-unstable"; # for packages on the edge

    home-manager.url = "github:nix-community/home-manager/release-22.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";

    # Discord
    discord-overlay.url = "github:InternetUnexplorer/discord-overlay";
    replugged.url = "github:LunNova/replugged-nix-flake";
    # discord-tweaks = {
    #   url = "github:NurMarvin/discord-tweaks";
    #   flake = false;
    # };
    # discord-image-tools = {
    #   url = "github:powerfart-plugins/image-tools";
    #   flake = false;
    # };
    # discord-push-fix = {
    #   url = "github:Karamu98/AlwaysPushNotifications";
    #   flake = false;
    # };
    # discord-better-status-indicators = {
    #   url = "github:griefmodz/better-status-indicators";
    #   flake = false;
    # };
    # discord-multitask = {
    #   url = "github:powercord-community/multitask";
    #   flake = false;
    # };
    # discord-view-raw = {
    #   url = "github:Juby210/view-raw";
    #   flake = false;
    # };
    # discord-channel-typing = {
    #   url = "github:powercord-community/channel-typing";
    #   flake = false;
    # };

    # Spotify
    spicetify-nix.url = "github:the-argus/spicetify-nix";
    spicetify-nix.inputs.nixpkgs.follows = "nixpkgs";

    # XMonad
    xmonad-contrib.url = "github:xmonad/xmonad-contrib";

    # Extras
    nixos-hardware.url = "github:nixos/nixos-hardware";
    nur.url = "github:nix-community/NUR";
    adblock.url = "github:StevenBlack/hosts";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    nixpkgs-unstable,
    adblock,
    ...
  }: let
    inherit (lib.my) mapModules mapModulesRec mapHosts;

    system = "x86_64-linux";

    mkPkgs = pkgs: extraOverlays:
      import pkgs {
        inherit system;
        config.allowUnfree = true; # forgive me Stallman senpai
        config.permittedInsecurePackages = ["electron-13.6.9"];
        overlays = extraOverlays ++ (lib.attrValues self.overlays);
      };
    pkgs = mkPkgs nixpkgs [self.overlay];
    pkgs' = mkPkgs nixpkgs-unstable [];

    lib = nixpkgs.lib.extend (self: super: {
      my = import ./lib {
        inherit pkgs inputs;
        lib = self;
      };
    });
  in {
    lib = lib.my;

    overlay = final: prev: {
      unstable = pkgs';
      my = self.packages."${system}";
    };

    overlays = mapModules ./overlays import;

    packages."${system}" = mapModules ./packages (p: pkgs.callPackage p {});

    nixosModules =
      {
        dotfiles = import ./.;
      }
      // mapModulesRec ./modules import;

    nixosConfigurations = mapHosts ./hosts {};

    devShells."${system}".default = import ./shell.nix {
      inherit pkgs;
      # pre-commit-hook = self.checks.${system}.pre-commit-check;
    };

    # checks.${system}.pre-commit-check = inputs.pre-commit-hooks.lib.${system}.run {
    #   src = ./.;
    #   hooks = {
    #     alejandra.enable = true;
    #     statix.enable = true;
    #     # nixfmt.enable = true;
    #     shellcheck.enable = true;
    #   };
    # };

    templates = {
      full = {
        path = ./.;
        description = "A grossly incandescent nixos config";
      };
      minimal = {
        path = ./templates/minimal;
        description = "A grossly incandescent and minimal nixos config";
      };
    };
    defaultTemplate = self.templates.minimal;

    defaultApp."${system}" = {
      type = "app";
      program = ./bin/hey;
    };
  };
}
