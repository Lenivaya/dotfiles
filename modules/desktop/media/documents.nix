# modules/desktop/media/docs.nix

{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.media.documents;
  configDir = config.dotfiles.configDir;
in
{
  options.modules.desktop.media.documents = {
    enable = mkBoolOpt false;
    pdf.enable = mkBoolOpt false;
    ebook.enable = mkBoolOpt false;
    latex.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs;
      (mkMerge [
        [ libreoffice-fresh ]

        (mkIf cfg.ebook.enable [ calibre foliate ])
        (mkIf cfg.pdf.enable [ evince zathura ])
        (mkIf cfg.latex.enable [
          texlab
          texlive.combined.scheme-full # FULL
          rubber
        ])
      ]);

    home.configFile."zathura" = mkIf cfg.pdf.enable {
      source = "${configDir}/zathura";
      recursive = true;
    };
  };
}
