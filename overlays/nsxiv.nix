_final: prev: rec {
  sxiv = nsxiv;
  nsxiv = prev.nsxiv.overrideAttrs (
    _oa:
    let
      version = "33";
      rev = "v${version}";

      nsxiv-extra = prev.fetchFromGitHub {
        owner = "nsxiv";
        repo = "nsxiv-extra";
        rev = "24384847326ad44ac98b7ee7e6fbfa02548ca9c0";
        hash = "sha256-hB8HK6uyoecnjTuBDKrqg1q/VFDpRWPGWXGmQion5rg=";
      };
    in
    {
      inherit version;

      src = prev.fetchFromGitHub {
        inherit rev;
        owner = "nsxiv";
        repo = "nsxiv";
        hash = "sha256-H1s+pLpHTmoDssdudtAq6Ru0jwZZ55/qamEVgtHTGfk=";
      };

      patches = [
        "${nsxiv-extra}/patches/dmenu-search/dmenu-search-${rev}.patch"
        "${nsxiv-extra}/patches/color-invert/color-invert-95bc9b4.diff"
        # "${nsxiv-extra}/patches/dmenu-mode/dmenu-mode-${rev}.patch"
        # "${nsxiv-extra}/patches/toggle-winbg/toggle-winbg-${rev}.diff"
        # "${nsxiv-extra}/patches/random-image/random-image-${rev}.diff"
      ];

      postInstall = ''
        ln -s "$out/bin/nsxiv" "$out/bin/sxiv"
      '';
    }
  );
}
