{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.haskell;
in {
  options.modules.dev.haskell = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs;
      [
        (haskellPackages.ghcWithPackages
          (ps: with ps; [ cabal-install stack hlint brittany ] # ghc-mod
          ))
      ];
  };

}
