{
  pkgs,
  lib,
  config,
  inputs,
  ...
}:
with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
in
{
  imports = [ inputs.nix-index-db.nixosModules.nix-index ];

  # I want to use nix-index only for comma, but
  # for shell checks command-not-found works much faster
  # than nix-index
  programs.nix-index-database.comma = mkForce enabled;
  programs.command-not-found.enable = mkForce true;
  programs.nix-index = {
    enableZshIntegration = mkForce false;
    enableBashIntegration = mkForce false;
  };
  # programs.nix-index.enable = mkForce false;

  modules.shell = {
    fzf = enabled;
  };

  # Some nice shell things
  user.packages = with pkgs; [
    bat
    fd
    fselect
    htop
    btop # htop but prettier
    tree
    thefuck
    navi
    starship
    zoxide
    atuin
    sysz
    translate-shell
    # carapace

    nix-your-shell # use configured shell in nix shells
  ];

  home.programs.eza = enabled // {
    icons = "auto";
    extraOptions = [ "--hyperlink" ];
  };

  home.programs.broot = enabled;

  home.programs.tealdeer = enabled // {
    settings = {
      display = {
        compact = false;
        use_pager = true;
      };
      updates = {
        auto_update = true;
      };
    };
  };

  home.configFile."starship.toml" = {
    source = "${configDir}/starship/starship.toml";
    recursive = true;
  };

  home.configFile."htop" = {
    source = "${configDir}/htop";
    recursive = true;
  };

  home.configFile."bat" = {
    source = "${configDir}/bat";
    recursive = true;
  };
}
