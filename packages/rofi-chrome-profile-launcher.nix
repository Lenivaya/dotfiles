{
  lib,
  stdenv,
  fetchFromGitHub,
  makeWrapper,
  python3,
  bash,
  ...
}:
stdenv.mkDerivation rec {
  pname = "rofi-chrome-profile-launcher";
  name = pname;

  src = fetchFromGitHub {
    owner = "claudiodangelis";
    repo = "rofi-chrome-profile-launcher";
    rev = "0b3a09cdbeef2aaa501ae833213127f721b3f90d";
    hash = "sha256-4A4VUYaTdWy/aTi6SdTMUdMiq9++smKUxMH6hepbPS0=";
  };

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    runHook preInstall

    mv rofi-chrome-profile-launcher.sh rofi-chrome-profile-launcher
    install -D --target-directory=$out/bin/ ./rofi-chrome-profile-launcher

    wrapProgram $out/bin/rofi-chrome-profile-launcher \
      --prefix PATH ":" ${
        lib.makeBinPath [
          python3
          bash
        ]
      }

    runHook postInstall
  '';
}
