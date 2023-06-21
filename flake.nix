{
  description = "Config...";

  # https://github.com/nix-community/haumea ?

  inputs = {
    # Core dependencies
    nixpkgs.url = "nixpkgs/nixos-23.05"; # primary nixpkgs
    nixpkgs-unstable.url = "nixpkgs/nixpkgs-unstable"; # for packages on the edge

    home-manager.url = "github:nix-community/home-manager/release-23.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";

    # Spotify
    spicetify-nix.url = "github:the-argus/spicetify-nix";
    spicetify-nix.inputs.nixpkgs.follows = "nixpkgs";

    # XMonad
    xmonad-contrib.url = "github:xmonad/xmonad-contrib";

    # vscode
    nix-vscode-extensions.url = "github:nix-community/nix-vscode-extensions";
    nix-vscode-extensions.inputs.nixpkgs.follows = "nixpkgs";

    # Nix cli helper
    nh.url = "github:viperML/nh";
    nh.inputs.nixpkgs.follows = "nixpkgs";

    # Formatting
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";

    # Extras
    nixos-hardware.url = "github:nixos/nixos-hardware";
    nur.url = "github:nix-community/NUR";
    adblock.url = "github:StevenBlack/hosts";
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    nixpkgs-unstable,
    treefmt-nix,
    ...
  }: let
    inherit (lib.my) mapModules mapModulesRec mapHosts;

    system = "x86_64-linux";

    mkPkgs = pkgs: extraOverlays:
      import pkgs {
        inherit system;
        config.allowUnfree = true; # forgive me Stallman senpai
        overlays = extraOverlays ++ (lib.attrValues self.overlays);
      };
    pkgs = mkPkgs nixpkgs [self.overlay];
    pkgs' = mkPkgs nixpkgs-unstable [];

    lib = nixpkgs.lib.extend (self: _super: {
      my = import ./lib {
        inherit pkgs inputs;
        lib = self;
      };
    });
  in {
    lib = lib.my;

    overlay = _final: _prev: {
      unstable = pkgs';
      my = self.packages."${system}";
    };

    overlays =
      mapModules ./overlays import;

    packages."${system}" =
      mapModules ./packages (p: pkgs.callPackage p {});

    nixosModules =
      {dotfiles = import ./.;} // mapModulesRec ./modules import;

    nixosConfigurations =
      mapHosts ./hosts {};

    devShells."${system}".default = import ./shell.nix {
      inherit pkgs;
    };

    apps."${system}".default = {
      type = "app";
      program = ./bin/hey;
    };

    formatter.${system} =
      treefmt-nix.lib.mkWrapper
      pkgs
      {
        projectRootFile = "flake.nix";

        programs = {
          alejandra.enable = true;
          deadnix.enable = true;
          shfmt.enable = true;
          black.enable = true;
          rufo.enable = true;
          mdsh.enable = true;
          yamlfmt.enable = true;
          prettier.enable = true;
        };
      };

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
    templates.default = self.templates.minimal;
  };
}
