{ config, lib, pkgs, inputs, home-manager, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.editors.emacs;
  configDir = config.dotfiles.configDir;
  editorScript = pkgs.writeScriptBin "emacseditor" ''
    #!${pkgs.runtimeShell}
    if [ -z "$1" ]; then
      exec emacsclient --create-frame --alternate-editor emacs
    else
      exec emacsclient --alternate-editor emacs "$@"
    fi
  '';
in
{
  options.modules.editors.emacs = {
    enable = mkBoolOpt false;
    default = mkBoolOpt false;
    doom = { enable = mkBoolOpt true; };
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [ inputs.emacs-overlay.overlay ];

    user.packages = with pkgs; [
      binutils # native-comp needs 'as', provided by this

      ## Doom dependencies
      git
      (ripgrep.override { withPCRE2 = true; })

      ## Optional dependencies
      editorconfig-core-c # per-project style config
      fd # faster projectile indexing
      gnutls # for TLS connectivity
      imagemagick # for image-dired
      (lib.mkIf (config.programs.gnupg.agent.enable)
        pinentry_emacs) # in-emacs gnupg prompts
      zstd # for undo-tree compression
      calibre # for calibredb
      python39Packages.pylatexenc # LaTeX parser
      # my.my_cookies # leetcode cookie retriever

      ## Module dependencies
      # :app everywhere
      xdotool
      xclip
      xorg.xwininfo
      xorg.xprop
      # :checkers spell
      (aspellWithDicts (ds: with ds; [ en en-computers en-science uk ru ]))
      # :checkers grammar
      languagetool
      # :tools lookup
      sqlite
      # :lang markdown previews
      python39Packages.grip
      # :term vterm
      libtool
      # wakatime
      wakatime
    ];

    env.PATH = [ "$HOME/.emacs.d/bin" ];

    fonts.fonts = [ pkgs.emacs-all-the-icons-fonts ];

    home.programs.emacs = {
      enable = true;
      package = pkgs.emacsGcc; # 28 + native-comp
      # pkgs.emacsPgtkGcc; # 28 + pgtk + native-comp
      extraPackages = epkgs:
        [
          # :term vterm
          epkgs.vterm
        ];
    };

    env.EDITOR =
      mkIf cfg.default (mkOverride 900 "${editorScript}/bin/emacseditor");

    # init.
    system.userActivationScripts.doomEmacs = mkIf cfg.doom.enable ''
      if ! [ -d $HOME/.emacs.d ]; then
            git clone https://github.com/hlissner/doom-emacs $HOME/.emacs.d
      fi
      if ! [ -d $HOME/.config/doom ]; then
            ln -s ${configDir}/doom ~/.config/doom
            # ${pkgs.emacs}/bin/emacs --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "~/.config/doom/config.org")'
            $HOME/.emacs.d/bin/org-tangle ~/.config/doom/config.org
      fi
    '';
  };
}
