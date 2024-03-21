{
  lib,
  stdenv,
  fetchFromGitHub,
  makeWrapper,
  dmenu,
  bash,
  jq,
  pipewire,
  wireplumber,
  ...
}:
stdenv.mkDerivation rec {
  pname = "dmenu-pipewire";
  version = "unstable-2023-08-02";

  src = fetchFromGitHub {
    owner = "hvitoi";
    repo = "dmenu-pipewire";
    rev = "5f760f073652db7ce6dbc2ffa4ded098cc4dede9";
    hash = "sha256-T8y/V/QaiVqv+LjaeeujweyawxYvqvy8q8iMX7KgZ04=";
  };

  nativeBuildInputs = [makeWrapper];

  prePatch = ''
    substituteInPlace ./dmenu-pipewire \
      --replace '/bin/bash' ${lib.getExe bash}
  '';

  installPhase = ''
    runHook preInstall

    install -D --target-directory=$out/bin/ ./dmenu-pipewire

    wrapProgram $out/bin/dmenu-pipewire \
      --prefix PATH ":" ${lib.makeBinPath [dmenu jq pipewire wireplumber]}

    runHook postInstall
  '';

  meta = with lib; {
    description = "Audio sink chooser";
    homepage = "https://github.com/hvitoi/dmenu-pipewire";
    license = licenses.mit;
    maintainers = with maintainers; [];
    mainProgram = "dmenu-pipewire";
    platforms = platforms.all;
  };
}
