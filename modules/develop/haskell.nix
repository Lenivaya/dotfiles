{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.haskell;
in {
  options.modules.dev.go = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    my.packages = with pkgs;
      [
        (haskellPackages.ghcWithPackages
          (ps: with ps; [ cabal-install stack hlint brittany ] # ghc-mod
          ))
      ];
  };

}
