{
  config,
  options,
  pkgs,
  lib,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.shell.zsh;
  inherit (config.dotfiles) configDir;
in {
  options.modules.shell.zsh = with types; {
    enable = mkBoolOpt false;

    aliases = mkOpt (attrsOf (either str path)) {};

    rcInit = mkOpt' lines "" ''
      Zsh lines to be written to $XDG_CONFIG_HOME/zsh/extra.zshrc and sourced by
      $XDG_CONFIG_HOME/zsh/.zshrc
    '';
    envInit = mkOpt' lines "" ''
      Zsh lines to be written to $XDG_CONFIG_HOME/zsh/extra.zshenv and sourced
      by $XDG_CONFIG_HOME/zsh/.zshenv
    '';

    rcFiles = mkOpt (listOf (either str path)) [];
    envFiles = mkOpt (listOf (either str path)) [];
  };

  config = mkIf cfg.enable {
    users.defaultUserShell = pkgs.zsh;

    programs.zsh =
      enabled
      // {
        # Slow
        enableGlobalCompInit = false;
        promptInit = "";
        setOptions = [];
      };
    programs.command-not-found.enable = false;

    # Some nice shell things
    user.packages = with pkgs; [
      zsh
      bat
      exa
      fd
      fzf
      htop
      btop # htop but prettier
      tree
      fasd
      thefuck
      navi
      starship
      zoxide
      atuin

      nix-your-shell # use zsh in nix shells
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
          updates = {
            auto_update = true;
          };
        };
      };

    home.configFile = {
      # Write it recursively so other modules can write files to it
      "zsh" = {
        source = "${configDir}/zsh";
        recursive = true;
      };

      # Why am I creating extra.zsh{rc,env} when I could be using extraInit?
      # Because extraInit generates those files in /etc/profile, and mine just
      # write the files to ~/.config/zsh; where it's easier to edit and tweak
      # them in case of issues or when experimenting.
      "zsh/extra.zshrc".text = let
        aliasLines = mapAttrsToList (n: v: ''alias ${n}="${v}"'') cfg.aliases;
      in ''
        # This file was autogenerated, do not edit it!
        ${concatStringsSep "\n" aliasLines}
        ${concatMapStrings (path: ''
            source '${path}'
          '')
          cfg.rcFiles}
        ${cfg.rcInit}
      '';

      "zsh/extra.zshenv".text = ''
        # This file is autogenerated, do not edit it!
        ${concatMapStrings (path: ''
            source '${path}'
          '')
          cfg.envFiles}
        ${cfg.envInit}
      '';
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

    env = {
      ZDOTDIR = "$XDG_CONFIG_HOME/zsh";
      ZSH_CACHE = "$XDG_CACHE_HOME/zsh";
    };
  };
}
