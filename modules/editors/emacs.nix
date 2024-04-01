{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.editors.emacs;
  inherit (config.dotfiles) configDir;

  editorScript = pkgs.writeScriptBin "emacseditor" ''
    #!${pkgs.runtimeShell}
    if [ -z "$1" ]; then
      exec emacsclient --create-frame --alternate-editor emacs
    else
      exec emacsclient --create-frame --alternate-editor emacs "$@"
    fi
  '';
in {
  options.modules.editors.emacs = {
    enable = mkBoolOpt false;
    default = mkBoolOpt false;
    doom = rec {
      enable = mkBoolOpt false;
      repoUrl = mkOpt types.str "https://github.com/hlissner/doom-emacs";
    };
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      binutils # native-comp needs 'as', provided by this

      ## Doom dependencies
      git
      (ripgrep.override {withPCRE2 = true;})

      ## Optional dependencies
      editorconfig-core-c # per-project style config
      fd # faster projectile indexing
      gnutls # for TLS connectivity
      imagemagick # for image-dired
      (
        mkIf
        config.programs.gnupg.agent.enable
        pinentry-emacs
      ) # in-emacs gnupg prompts
      zstd # for undo-tree compression

      python39Packages.pylatexenc # LaTeX parser
      ghostscript

      # my.my_cookies # leetcode cookie retriever

      ## Module dependencies
      # :app everywhere
      xdotool
      xclip
      xorg.xwininfo
      xorg.xprop

      # :checkers (spell +enchant)
      enchant
      (aspellWithDicts (ds:
        with ds; [
          en
          en-computers
          en-science
          uk
          ru
        ]))
      (hunspellWithDicts (
        with hunspellDicts; [
          en_US-large
          en_GB-large
          uk_UA
          ru_RU
        ]
      ))

      # :checkers grammar
      languagetool
      # :tools lookup
      sqlite
      # :lang markdown previews
      python311Packages.grip
      # :lang resc
      jq
      # :term vterm
      libtool
      # wakatime
      wakatime

      # mermaid diagrams in org-mode
      nodePackages.mermaid-cli
      # gnuplot in org mode
      gnuplot

      # :org roam
      graphviz

      # nice prose linting
      vale
    ];

    fonts.packages = with pkgs; [
      emacs-all-the-icons-fonts
    ];

    home.programs.emacs =
      enabled
      // {
        package =
          if config.modules.desktop.isWayland
          then pkgs.emacs29-pgtk
          else pkgs.emacs29-gtk3;
        extraPackages = epkgs: with epkgs; [vterm];
      };

    modules.editors.default = mkIf cfg.default (getExe editorScript);

    env.PATH = ["$XDG_CONFIG_HOME/emacs/bin"];

    system.userActivationScripts =
      mkIf cfg.doom.enable
      {
        installDoomEmacs = ''
          if ! [ -d ~/.config/emacs ]; then
              git clone --depth=1 --single-branch "${cfg.doom.repoUrl}" "$XDG_CONFIG_HOME/emacs"
          fi
        '';
        linkAndTangleDoomConfig = ''
          if ! [ -d $HOME/.config/doom ]; then
              ln -s ${configDir}/doom ~/.config/doom
              ~/.config/emacs/bin/org-tangle ~/.config/doom/config.org
          fi
        '';
      };
  };
}
