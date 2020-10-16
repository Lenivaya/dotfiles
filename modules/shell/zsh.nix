{ config, options, pkgs, lib, ... }:

with lib;
with lib.my;
with import <home-manager/modules/lib/dag.nix> { inherit lib; }; {
  options.modules.shell.zsh = with types; {
    aliases = mkOpt (attrsOf (either str path)) { };

    rcInit = mkOpt' lines "" ''
      Zsh lines to be written to $XDG_CONFIG_HOME/zsh/extra.zshrc and sourced by
      $XDG_CONFIG_HOME/zsh/.zshrc
    '';
    envInit = mkOpt' lines "" ''
      Zsh lines to be written to $XDG_CONFIG_HOME/zsh/extra.zshenv and sourced
      by $XDG_CONFIG_HOME/zsh/.zshenv
    '';

    rcFiles = mkOpt (listOf (either str path)) [ ];
    envFiles = mkOpt (listOf (either str path)) [ ];
  };

  # Some nice shell things
  user.packages = with pkgs;
    [ zsh bat exa fd fzf htop tldr tree fasd ]
    ++ (with pkgs.unstable; [ starship ]);

  my.home.programs.broot.enable = true;

  # my.home.xdg.configFile."zsh" = {
  #   source = <config/zsh>;
  #   recursive = true;
  # };

  ##  to change and dont rebuild all nixos
  my.home.home.activation.linkZshConfig = dagEntryAfter [ "writeBoundary" ] ''
    [ -d $XDG_CONFIG_HOME/zsh ] || ln -sf "$HOME/.dotfiles/config/zsh" $XDG_CONFIG_HOME/zsh
  '';

  home.configFile."starship.toml" = {
    source = <config/starship/starship.toml>;
    recursive = true;
  };

  home.configFile."htop" = {
    source = <config/htop>;
    recursive = true;
  };

  env = {
    ZDOTDIR = "$XDG_CONFIG_HOME/zsh";
    ZSH_CACHE = "$XDG_CACHE_HOME/zsh";
  };

  programs.zsh = {
    enable = true;

    # Slow
    enableGlobalCompInit = false;
    promptInit = "";
    setOptions = [ ];
  };
}
