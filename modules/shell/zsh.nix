{ pkgs, lib, ... }:
with lib;
with import <home-manager/modules/lib/dag.nix> { inherit lib; }; {

  # Some nice shell things
  my.packages = with pkgs;
    [ zsh bat exa fd fzf htop tldr tree ] ++ (with pkgs.unstable; [ starship ]);

  my.home.programs.broot.enable = true;


  # my.home.xdg.configFile."zsh" = {
  #   source = <config/zsh>;
  #   recursive = true;
  # };

  ##  to change and dont rebuild all nixos
  my.home.home.activation.linkZshConfig = dagEntryAfter [ "writeBoundary" ] ''
    [ -d $XDG_CONFIG_HOME/zsh ] || ln -sf "$HOME/.dotfiles/config/zsh" $XDG_CONFIG_HOME/zsh
  '';

  my.home.xdg.configFile."starship.toml" = {
    source = <config/starship/starship.toml>;
    recursive = true;
  };

  my.home.xdg.configFile."htop" = {
    source = <config/htop>;
    recursive = true;
  };

  my.env = {
    ZDOTDIR = "$XDG_CONFIG_HOME/zsh";
    ZSH_CACHE = "$XDG_CACHE_HOME/zsh";
  };

  programs.zsh = {
    enable = true;

    # Slow
    enableCompletion = false;
    enableGlobalCompInit = false;
    promptInit = "";
    setOptions = [ ];
  };
}
