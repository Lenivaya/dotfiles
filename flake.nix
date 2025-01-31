{
  description = "Config...";

  # https://github.com/nix-community/haumea ?

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable"; # primary nixpkgs
    nixpkgs-unstable.url = "nixpkgs/nixos-unstable"; # for packages on the edge

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    git-ignore-nix.url = "github:hercules-ci/gitignore.nix/master";

    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";

    spicetify-nix = {
      url = "github:gerg-l/spicetify-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    xmonad = {
      url = "github:xmonad/xmonad";
      inputs.unstable.follows = "nixpkgs";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.git-ignore-nix.follows = "git-ignore-nix";
    };
    xmonad-contrib = {
      url = "github:xmonad/xmonad-contrib";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.xmonad.follows = "xmonad";
      inputs.git-ignore-nix.follows = "git-ignore-nix";
    };
    xmonad-extras = {
      url = "github:xmonad/xmonad-extras";
      flake = false;
    };

    nix-vscode-extensions.url = "github:nix-community/nix-vscode-extensions";

    srvos = {
      url = "github:nix-community/srvos";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    pre-commit-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # chaotic.url = "github:chaotic-cx/nyx/nyxpkgs-unstable";
    chaotic.url = "github:chaotic-cx/nyx";

    firefox = {
      url = "github:colemickens/flake-firefox-nightly";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    betterfox = {
      url = "github:yokoffing/betterfox";
      flake = false;
    };
    firefox-csshacks = {
      url = "github:MrOtherGuy/firefox-csshacks";
      flake = false;
    };

    nixos-hardware.url = "github:nixos/nixos-hardware";
    nixos-facter-modules.url = "github:numtide/nixos-facter-modules";
    nur.url = "github:nix-community/NUR";

    programsdb = {
      url = "github:wamserma/flake-programs-sqlite";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-index-db = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # IntelliJ
    intellimacs = {
      url = "github:MarcoIeni/intellimacs";
      flake = false;
    };

    # t480 fingerprints
    nixos-06cb-009a-fingerprint-sensor = {
      url = "github:bmanuel/nixos-06cb-009a-fingerprint-sensor";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    winapps = {
      url = "github:winapps-org/winapps";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    infuse = {
      url = "git+https://codeberg.org/amjoseph/infuse.nix";
      flake = false;
    };

    # some upstream things
    auto-cpufreq = {
      url = "github:AdnanHodzic/auto-cpufreq";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    skippy-xd = {
      url = "github:felixfung/skippy-xd";
      flake = false;
    };
    browser-previews = {
      url = "github:nix-community/browser-previews";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    resterrs = {
      # url = "path:///home/leniviy/code/Projects/resterrs";
      url = "github:Lenivaya/resterrs";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    stevenblack-hosts = {
      url = "github:StevenBlack/hosts";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  nixConfig = {
    extra-substituters = [
      "https://nix-community.cachix.org"
      "https://nixpkgs-unfree.cachix.org/"
      "https://cuda-maintainers.cachix.org"
      "https://nixpkgs-unfree.cachix.org" # unfree-package cache
      "https://numtide.cachix.org" # another unfree package cache
      "https://pre-commit-hooks.cachix.org"
      "https://viperml.cachix.org/"
      "https://chaotic-nyx.cachix.org/"

    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "nixpkgs-unfree.cachix.org-1:hqvoInulhbV4nJ9yJOEr+4wxhDV4xq2d1DK7S6Nj6rs="
      "cuda-maintainers.cachix.org-1:0dq3bujKpuEPMCX6U4WylrUDZ9JyUG0VpVZa7CNfq5E="
      "nixpkgs-unfree.cachix.org-1:hqvoInulhbV4nJ9yJOEr+4wxhDV4xq2d1DK7S6Nj6rs="
      "numtide.cachix.org-1:2ps1kLBUWjxIneOy1Ik6cQjb41X0iXVXeHigGmycPPE="
      "pre-commit-hooks.cachix.org-1:Pkk3Panw5AW24TOv6kz3PvLhlH8puAsJTBbOPmBo7Rc="
      "viperml.cachix.org-1:qZhKBMTfmcLL+OG6fj/hzsMEedgKvZVFRRAhq7j8Vh8="
      "chaotic-nyx.cachix.org-1:HfnXSw4pj95iI/n17rIDy40agHj12WfF+Gqk6SonIT8="
    ];
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      nixpkgs-unstable,
      treefmt-nix,
      home-manager,
      ...
    }:
    let
      system = "x86_64-linux";

      inherit (lib.my) mapModules mapModulesRec mapHosts;

      mkPkgs =
        pkgs: extraOverlays:
        import pkgs {
          inherit system;
          config.allowUnfree = true;
          config.nvidia.acceptLicense = true;
          config.permittedInsecurePackages = [
            "aspnetcore-runtime-6.0.36"
            "aspnetcore-runtime-wrapped-6.0.36"
            "dotnet-sdk-6.0.428"
            "dotnet-sdk-wrapped-6.0.428"
            "deskflow-1.18.0"
          ];
          overlays = extraOverlays ++ (lib.attrValues self.overlays);
        };
      pkgs = mkPkgs nixpkgs [ self.overlay ];
      pkgs' = mkPkgs nixpkgs-unstable [ ];

      lib = nixpkgs.lib.extend (
        self: _super:
        {
          my = import ./lib {
            inherit pkgs inputs system;
            lib = self;
          };
          inherit ((import inputs.infuse { inherit lib; }).v1) infuse;
        }
        // home-manager.lib
      );
    in
    {
      lib = lib.my;

      overlay = _final: _prev: {
        unstable = pkgs';
        my = self.packages."${system}";
      };
      overlays = mapModules ./overlays import;
      packages."${system}" = mapModules ./packages (p: pkgs.callPackage p { inherit inputs; });

      nixosModules = {
        dotfiles = import ./.;
      } // mapModulesRec ./modules import;
      nixosConfigurations = mapHosts ./hosts { };

      devShells."${system}".default = import ./shell.nix { inherit pkgs inputs system; };
      formatter.${system} = import ./extra/treefmt.nix {
        treefmt-wrapper = treefmt-nix.lib.mkWrapper pkgs;
      };
    };
}
