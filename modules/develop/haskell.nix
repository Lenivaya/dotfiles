{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.haskell;
in
{
  options.modules.dev.haskell = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs;
      [
        # haskell-language-server

        (haskellPackages.ghcWithHoogle (ps:
          with ps; [
            stack
            cabal-install
            hlint
            brittany
            hasktags
            haskell-language-server
          ]))
      ];
  };

}
