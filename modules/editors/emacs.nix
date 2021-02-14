{ config, lib, pkgs, inputs, ... }:

with lib;
with lib.my;
let cfg = config.modules.editors.emacs;
in {
  options.modules.editors.emacs = {
    enable = mkBoolOpt false;
    default = mkBoolOpt false;
    doom = { enable = mkBoolOpt true; };
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      emacs
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

      ## Module dependencies
      # :app everywhere
      xdotool
      xclip
      xorg.xwininfo
      xorg.xprop
      # :checkers spell
      (aspellWithDicts (ds: with ds; [ en en-computers en-science ]))
      # :checkers grammar
      languagetool
      # :tools lookup
      sqlite
      # :lang markdown previews
      python37Packages.grip
      # :lang latex & :lang org (latex previews)
      texlab
      texlive.combined.scheme-full # FULL
    ];

    env.PATH = [ "$HOME/.emacs.d/bin" ];

    fonts.fonts = [ pkgs.emacs-all-the-icons-fonts ];

    services.emacs = {
      enable = true;
      defaultEditor = mkIf cfg.default true;
    };

    init.doomEmacs = mkIf cfg.doom.enable ''
      if [ -d $HOME/.config/emacs ]; then
            git clone https://github.com/hlissner/doom-emacs $HOME/.emacs.d
      fi
      if [ -d $HOME/.config/doom ]; then
            ln -s ${configDir}/doom ~/.config/doom
            ${pkgs.emacs}/bin/emacs --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "~/.config/doom/config.org")'
      fi
    '';
  };
}
