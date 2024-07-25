_final: prev: rec {
  sxiv = nsxiv;
  nsxiv = prev.nsxiv.overrideAttrs (
    _oa:
    let
      version = "31";
      rev = "v${version}";

      nsxiv-extra = prev.fetchFromGitHub {
        owner = "nsxiv";
        repo = "nsxiv-extra";
        rev = "0e02fad7682be215376f8920fc03d623a8c838da";
        hash = "sha256-DQRU+20F+5880ENZuFXugVisP0QBB64Tm/0MkE7/Kzg=";
      };
    in
    {
      inherit version;

      src = prev.fetchFromGitHub {
        inherit rev;
        owner = "nsxiv";
        repo = "nsxiv";
        hash = "sha256-X1ZMr5OADs9GIe/kp/kEqKMMHZMymd58m9+f0SPzn7s=";
      };

      patches = [
        "${nsxiv-extra}/patches/dmenu-mode/dmenu-mode-${rev}.patch"
        "${nsxiv-extra}/patches/dmenu-search/dmenu-search-${rev}.patch"
        "${nsxiv-extra}/patches/toggle-winbg/toggle-winbg-${rev}.diff"
        "${nsxiv-extra}/patches/color-invert/color-invert-95bc9b4.diff"
        # "${nsxiv-extra}/patches/random-image/random-image-${rev}.diff"
      ];

      postInstall = ''
        ln -s "$out/bin/nsxiv" "$out/bin/sxiv"
      '';
    }
  );
}
