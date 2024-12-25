{
  pkgs,
  inputs,
  system,
  ...
}:
rec {
  optimizeWithFlags =
    pkg: flags:
    pkgs.lib.overrideDerivation pkg (
      old:
      let
        newflags = pkgs.lib.foldl' (acc: x: "${acc} ${x}") "" flags;
        oldflags = if (pkgs.lib.hasAttr "NIX_CFLAGS_COMPILE" old) then "${old.NIX_CFLAGS_COMPILE}" else "";
      in
      {
        NIX_CFLAGS_COMPILE = "${oldflags} ${newflags}";
      }
    );

  withClang =
    pkg:
    pkgs.lib.overrideDerivation pkg (_oa: {
      stdenv = pkgs.clangStdenv;
    });

  withFastGCC =
    pkg:
    pkgs.lib.overrideDerivation pkg (_oa: {
      stdenv = pkgs.fastStdenv;
    });

  withNativeOptimizations =
    pkg:
    pkgs.lib.overrideDerivation pkg (oa: {
      stdenv = pkgs.impureUseNativeOptimizations oa.stdenv;
    });

  withThinLTO = pkg: optimizeWithFlags pkg [ "-flto=thin" ];

  optimizeForThisHost =
    pkg:
    withNativeOptimizations (
      optimizeWithFlags pkg [
        "-O3"
        "-march=native"
        "-fPIC"
      ]
    );

  withDebuggingCompiled = pkg: optimizeWithFlags pkg [ "-DDEBUG" ];

  useNixpkgs = nixpkgs: import nixpkgs { inherit system; };
  fromUnstable = useNixpkgs (inputs.nixpkgs-unstable or inputs.nixpkgs);
  fromRev =
    rev: hash:
    useNixpkgs (
      inputs.nixpkgs.legacyPackages.${system}.fetchFromGitHub {
        owner = "NixOS";
        repo = "nixpkgs";
        inherit rev hash;
      }
    );
  fromPR = pr: fromRev "refs/pull/${toString pr}/head";
}
