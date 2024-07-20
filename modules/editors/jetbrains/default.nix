# https://scriptedalchemy.medium.com/speeding-up-intellij-webstorm-and-other-jetbrains-products-96a2abe6bf2
# https://gist.github.com/HELWATANY/c86ac31128468267c16ff0ef9f740637
{
  config,
  lib,
  inputs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.editors.jetbrains;
  inherit (config.dotfiles) outOfStoreConfigDir;
in {
  options.modules.editors.jetbrains = with types; {
    enable = mkBoolOpt false;
    packages = mkOpt (listOf package) [];
  };

  config = mkIf cfg.enable {
    user.packages = cfg.packages;

    system.userActivationScripts.linkIdeaVimRc =
      linkIfNotExist "~/.ideavimrc" "${outOfStoreConfigDir}/jetbrains/.ideavimrc";

    home.file.".intellimacs".source = inputs.intellimacs;
  };
}
