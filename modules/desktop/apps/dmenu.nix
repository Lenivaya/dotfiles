{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.dmenu;
in {
  options.modules.desktop.apps.dmenu.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs;
      [
        (dmenu.overrideAttrs (oldAttr: {
          src = fetchgit {
            url = "https://github.com/LukeSmithxyz/dmenu";
            sha256 = "qwOcJqYGMftFwayfYA3XM0xaOo6ALV4gu1HpFRapbFg=";
          };
        }))
      ];
  };
}
