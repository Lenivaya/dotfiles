{ config, lib, pkgs, inputs, home-manager, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.editors.emacs;
  emacsPkg = pkgs.emacsPgtkGcc; # 28 + pgtk + native-comp
  editorScript = pkgs.writeScriptBin "emacseditor" ''
    #!${pkgs.runtimeShell}
    if [ -z "$1" ]; then
      exec emacsclient --create-frame --alternate-editor emacs
    else
      exec emacsclient --alternate-editor emacs "$@"
    fi
  '';
in {
  options.modules.editors.emacs = {
    enable = mkBoolOpt false;
    default = mkBoolOpt false;
    doom = { enable = mkBoolOpt true; };
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [ inputs.emacs-overlay.overlay ];

    user.packages = with pkgs; [
      binutils # native-comp needs 'as', provided by this
      emacsPkg

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

    # services.emacs = {
    #   enable = true;
    #   defaultEditor = mkIf cfg.default true;
    # };

    home-manager.users.${config.user.name}.services.emacs = {
      enable = true;
      package = emacsPkg;
      client.enable = true;
      socketActivation.enable = true;
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
