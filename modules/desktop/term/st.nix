{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.term.st;
in {
  options.modules.desktop.term.st = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      (st.overrideAttrs (oldAttrs: {
        buildInputs = with pkgs.xorg;
          [ libX11 libXft libXcursor ] ++ (with pkgs; [ harfbuzz ]);
        src = fetchgit { url = "https://github.com/Lenivaya/st"; sha256 = "pZdvcia6T07r3mVAszSQx3/oeZbAfOBymFKo1YumAg8="; };
      }))

      (dmenu.overrideAttrs (oldAttr: {
        src = fetchgit { url = "https://github.com/LukeSmithxyz/dmenu"; sha256 = "NJ6oiSLprh6OzW2KefRry77O3tQ0VsPFTJr8ySOjy4U="; };
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
