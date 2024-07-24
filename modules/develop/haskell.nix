{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  inherit (config.dotfiles) configDir;
  cfg = config.modules.dev.haskell;
in {
  options.modules.dev.haskell.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    home.file.".ghci".source = "${configDir}/ghci/.ghci";

    user.packages = with pkgs; [
      niv

      (haskellPackages.ghcWithHoogle (hpkgs:
        with hpkgs; [
          stack
          cabal-install
          cabal-fmt

          # Linter and formatters
          hlint
          # brittany FIXME broken
          ormolu
          fourmolu

          hasktags
          haskell-language-server
        ]))
    ];

    env = {
      CABAL_CONFIG = "$XDG_CONFIG_HOME/cabal/config";
      CABAL_DIR = "$XDG_DATA_HOME/cabal";
      STACK_ROOT = "$XDG_DATA_HOME/stack";
    };
  };
}
