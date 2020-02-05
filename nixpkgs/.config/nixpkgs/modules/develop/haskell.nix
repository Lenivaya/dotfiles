{ pkgs, ... }:

{
  home.packages = with pkgs; [
     (haskellPackages.ghcWithPackages(ps: with ps;
      [ cabal-install stack hoogle
        ghcid hlint ] # ghc-mod
    ))
  ];
}
