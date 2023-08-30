{
  pkgs,
  lib,
  config,
  ...
}:
with lib;
with lib.my; let
  inherit (config.dotfiles) configDir;
in {
  modules.shell = {
    fzf = enabled;
  };

  # Some nice shell things
  user.packages = with pkgs; [
    bat
    exa
    fd
    htop
    btop # htop but prettier
    tree
    thefuck
    navi
    starship
    zoxide
    atuin
    sysz

    nix-your-shell # use configured shell in nix shells
  ];

  home.programs.broot = enabled;

  home.programs.tealdeer =
    enabled
    // {
      settings = {
        display = {
          compact = false;
          use_pager = true;
        };
        updates = {auto_update = true;};
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
