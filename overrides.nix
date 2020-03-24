{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    (st.overrideAttrs (oldAttrs: {
      buildInputs = with pkgs.xorg; [ libX11 libXft libXcursor ];
      name = "custom-st";
      src = fetchGit {
        url = "https://github.com/Lenivaya/st";
      };
    }))

    (dmenu.overrideAttrs (oldAttr: {
      name = "custom-dmenu";
      src = fetchGit {
        url = "https://github.com/LukeSmithxyz/dmenu";
      };
    }))

  ];
}
