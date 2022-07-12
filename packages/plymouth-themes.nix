{
  stdenv,
  pkgs,
  theme ? "abstract_ring",
}:
with stdenv;
with pkgs;
  mkDerivation {
    name = "plymouth-themes-${theme}";

    src = fetchFromGitHub {
      owner = "adi1090x";
      repo = "plymouth-themes";
      rev = "bf2f570bee8e84c5c20caac353cbe1d811a4745f";
      sha256 = "VNGvA8ujwjpC2rTVZKrXni2GjfiZk7AgAn4ZB4Baj2k=";
    };

    installPhase = ''
      mkdir -p $out/share/plymouth/themes
      cp -r pack_**/${theme} $out/share/plymouth/themes
      substituteInPlace $out/share/plymouth/themes/${theme}/${theme}.plymouth \
        --replace "/usr" "$out"
    '';

    meta = with lib; {
      description = "A huge collection (80+) of plymouth themes ported from android bootanimations";
      homepage = "https://github.com/adi1090x/plymouth-themes";
      license = licenses.gpl3;
      platforms = platforms.linux;
    };
  }
