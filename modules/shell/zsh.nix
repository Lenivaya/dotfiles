{ pkgs, libs, ... }: {
  my.packages = with pkgs;
    [ zsh bat exa fd fzf htop tldr tree broot ]
    ++ (with pkgs.unstable; [ starship ]);

  my.home.xdg.configFile."zsh" = {
    source = <config/zsh>;
    recursive = true;
  };

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
