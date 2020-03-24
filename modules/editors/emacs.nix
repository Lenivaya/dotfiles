{ config, lib, pkgs, ... }:

{
  my = {
    packages = with pkgs; [
      ## Doom dependencies
      emacs
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

      ## Module dependencies
      # :checkers spell
      aspell
      aspellDicts.en
      aspellDicts.en-computers
      aspellDicts.en-science
      # :checkers grammar
      languagetool
      # :tools lookup
      sqlite
      # :lang cc
      ccls
      # :lang javascript
      nodePackages.javascript-typescript-langserver
      # :lang latex & :lang org (latex previews)
      texlive.combined.scheme-medium
      # :lang rust
      rustfmt
      rls
    ];

    env.PATH = [ "$HOME/.emacs.d/bin" ];
  };

  services.emacs = {
    enable = true;
    defaultEditor = true;
  };

  # Emacs-doom config
  my.home.home.file.".doom.d" = {
    source = <config/doom>;
    recursive = true;
  };

  fonts.fonts = [ pkgs.emacs-all-the-icons-fonts ];
}
