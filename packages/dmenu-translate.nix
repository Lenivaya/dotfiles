{
  lib,
  stdenv,
  fetchFromGitHub,
  makeWrapper,
  dmenu,
  translate-shell,
  libnotify,
}:
stdenv.mkDerivation rec {
  pname = "dmenu-translate";
  version = "unstable-2023-05-26";

  src = fetchFromGitHub {
    owner = "NikitaIvanovV";
    repo = "dmenu-translate";
    rev = "8cc717d3d3bec7f1d7db804e1c339362536b6e45";
    hash = "sha256-I38V51BnEH2LsUCIyRmOwnF737yaYDGiH/wsZhdGOHw=";
  };

  nativeBuildInputs = [makeWrapper];

  prePatch = ''
    substituteInPlace ./dmenu-translate \
      --replace '{DMENU} -i' {DMENU}
  '';

  installPhase = ''
    runHook preInstall

    install -D --target-directory=$out/bin/ ./dmenu-translate

    wrapProgram $out/bin/dmenu-translate \
      --prefix PATH ":" ${lib.makeBinPath [
      dmenu
      translate-shell
      libnotify
    ]}

    runHook postInstall
  '';

  meta = with lib; {
    description = "Quick text translation with dmenu";
    homepage = "https://github.com/NikitaIvanovV/dmenu-translate?tab=readme-ov-file";
    license = licenses.mit;
    maintainers = with maintainers; [];
    mainProgram = "dmenu-translate";
    platforms = platforms.all;
  };
}
