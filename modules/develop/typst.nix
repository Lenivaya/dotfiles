{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.dev.typst;
in
{
  options.modules.dev.typst.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      typst
      tinymist
      typst-live
      typst-lsp
      typstyle
    ];
  };
}
