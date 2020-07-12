{ config, options, lib, pkgs, ... }:

with lib; {
  options.modules.desktop.term.st = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf config.modules.desktop.term.st.enable {
    environment.systemPackages = with pkgs; [
      (st.overrideAttrs (oldAttrs: {
        buildInputs = with pkgs.xorg;
          [ libX11 libXft libXcursor ] ++ (with pkgs; [ harfbuzz ]);
        src = fetchGit { url = "https://github.com/Lenivaya/st"; };
      }))

      (dmenu.overrideAttrs (oldAttr: {
        src = fetchGit { url = "https://github.com/LukeSmithxyz/dmenu"; };
      }))

      (makeDesktopItem {
        name = "st";
        desktopName = "Suckless Terminal";
        genericName = "Default terminal";
        icon = "utilities-terminal";
        exec = "st";
        categories = "Development;System;Utility";
      })
    ];
  };
}
