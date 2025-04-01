# modules/desktop/media/docs.nix
{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.desktop.media.documents;
  inherit (config.dotfiles) configDir;
in
{
  options.modules.desktop.media.documents = {
    enable = mkBoolOpt false;
    writing.enable = mkBoolOpt false;
    pdf.enable = mkBoolOpt false;
    ebook.enable = mkBoolOpt false;
    latex.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages =
      with pkgs;
      (mkMerge [
        [
          pandoc # Universal Markup converter
        ]

        (mkIf cfg.writing.enable [
          libreoffice-fresh
        ])

        (mkIf cfg.ebook.enable [
          # calibre
          readest
        ])
        (mkIf cfg.pdf.enable [
          # evince
          papers
          zathura
        ])
        (mkIf cfg.latex.enable [
          texlab

          texlive.combined.scheme-full # FULL
          tectonic
          watchexec
        ])
      ]);

    home.configFile."zathura" = mkIf cfg.pdf.enable {
      source = "${configDir}/zathura";
      recursive = true;
    };
  };
}
