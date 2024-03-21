{
  fetchFromGitHub,
  python3Packages,
  stdenvNoCC,
  ...
}:
stdenvNoCC.mkDerivation rec {
  pname = "mpv-autosub";
  version = "git";

  src = fetchFromGitHub {
    owner = "davidde";
    repo = pname;
    rev = "35115355bd339681f97d067538356c29e5b14afa";
    hash = "sha256-BKT/Tzwl5ZA4fbdc/cxz0+CYc1zyY/KOXc58x5GYow0=";
  };

  postPatch = ''
    substituteInPlace autosub.lua \
      --replace '/home/david/.local/bin/subliminal' \
      '${python3Packages.subliminal}/bin/subliminal'
  '';

  installPhase = ''
    install -Dm644 autosub.lua $out/share/mpv/scripts/autosub.lua
  '';

  passthru.scriptName = "autosub.lua";
}
