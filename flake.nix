{
  description = "Config...";

  # https://github.com/nix-community/haumea ?

  inputs = {
    # Core dependencies
    nixpkgs.url = "nixpkgs/nixos-unstable"; # primary nixpkgs
    nixpkgs-unstable.url = "nixpkgs/nixos-unstable"; # for packages on the edge

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";

    # Spotify
    spicetify-nix = {
      url = "github:the-argus/spicetify-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # XMonad
    xmonad-contrib = {
      url = "github:xmonad/xmonad-contrib";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # vscode
    nix-vscode-extensions.url = "github:nix-community/nix-vscode-extensions";

    # Nix cli helper
    # nh.url = "github:viperML/nh";

    srvos = {
      url = "github:nix-community/srvos";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Formatting and pre-commit hooks
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Some interesting packages
    # https://github.com/chaotic-cx/nyx/pull/767 TODO
    chaotic.url = "github:chaotic-cx/nyx/nyxpkgs-unstable";

    betterfox = {
      url = "github:yokoffing/betterfox";
      flake = false;
    };

    browser-previews = {
      url = "github:nix-community/browser-previews";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Extras
    nixos-hardware.url = "github:nixos/nixos-hardware";
    nur.url = "github:nix-community/NUR";
    programsdb = {
      url = "github:wamserma/flake-programs-sqlite";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-index-db = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # some upstream things
    picom = {
      url = "github:yshui/picom?rev=05c764c6055570f5f3d648bf740608763c2b2348";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    skippy-xd = {
      # url = "github:felixfung/skippy-xd";
      url = "path:///home/leniviy/code/Projects/skippy-xd";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    nixpkgs-unstable,
    treefmt-nix,
    pre-commit-hooks,
    home-manager,
    ...
  }: let
    inherit (lib.my) mapModules mapModulesRec mapHosts;

    system = "x86_64-linux";

    mkPkgs = pkgs: extraOverlays:
      import pkgs {
        inherit system;
        config.allowUnfree = true; # forgive me Stallman senpai
        config.nvidia.acceptLicense = true;
        # devtunnel
        config.permittedInsecurePackages = [
          "openssl-1.1.1w"
        ];
        overlays = extraOverlays ++ (lib.attrValues self.overlays);
      };
    pkgs = mkPkgs nixpkgs [self.overlay];
    pkgs' = mkPkgs nixpkgs-unstable [];

    lib =
      nixpkgs.lib.extend
      (self: _super:
        {
          my = import ./lib {
            inherit pkgs inputs;
            lib = self;
          };
        }
        // home-manager.lib);
  in {
    lib = lib.my;

    overlay = _final: _prev: {
      unstable = pkgs';
      my = self.packages."${system}";
    };

    overlays =
      mapModules ./overlays import;

    packages."${system}" =
      mapModules ./packages (p: pkgs.callPackage p {inherit inputs;});

    nixosModules =
      {dotfiles = import ./.;} // mapModulesRec ./modules import;

    nixosConfigurations =
      mapHosts ./hosts {};

    devShells."${system}".default = let
      preCommitHook =
        (pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            editorconfig-checker.enable = true;
            black.enable = true;
            prettier.enable = true;
            shellcheck.enable = true;
            shfmt.enable = true;
            # yamllint.enable = true;
            actionlint.enable = true;
            alejandra.enable = true;
            deadnix.enable = true;
            # statix.enable = true;
            # convco.enable = true;
            # fourmolu.enable = true;
            typos = {
              enable = true;
              types = ["text"];
            };
          };
          settings.deadnix.edit = true;
        })
        .shellHook;
    in
      import ./shell.nix {inherit pkgs preCommitHook;};

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
