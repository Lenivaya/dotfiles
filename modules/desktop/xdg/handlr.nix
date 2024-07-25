# Default xdg-open from xdg-utils sometimes works like
# pure garbage. This is a simple wrapper that uses handlr-regex
# instead [1].
#
# [1]: https://wiki.archlinux.org/title/default_applications#handlr
{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.desktop.xdg.handlr;

  shadowedXdgOpen = pkgs.writeShellScriptBin "xdg-open" ''
    ${getExe pkgs.handlr-regex} open "$@"
  '';
in
{
  options.modules.desktop.xdg.handlr.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      handlr-regex
      shadowedXdgOpen
    ];
  };
}
