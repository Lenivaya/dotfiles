{
  lib,
  stdenv,
  fetchFromGitHub,
  makeWrapper,
  libnotify,
  udiskie,
  udisks,
  dmenu,
  ...
}:
stdenv.mkDerivation rec {
  pname = "udiskie-dmenu";
  version = "unstable-2023-01-30";

  src = fetchFromGitHub {
    owner = "fogine";
    repo = "udiskie-dmenu";
    rev = "61d7642542c5def6129c660d080c13822d76ed8e";
    hash = "sha256-GIfr0CkfpORjc/y8cZkoLKg0+M43Noeui4NGVzKfqaM=";
  };

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    runHook preInstall

    install -D --target-directory=$out/bin/ ./udiskie-dmenu

    wrapProgram $out/bin/udiskie-dmenu \
      --prefix PATH ":" ${
        lib.makeBinPath [
          libnotify
          udiskie
          udisks
          dmenu
        ]
      }

    runHook postInstall
  '';

  meta = with lib; {
    description = "Manage removable devices in couple of keystrokes";
    homepage = "https://github.com/fogine/udiskie-dmenu";
    changelog = "https://github.com/fogine/udiskie-dmenu/blob/${src.rev}/CHANGELOG.md";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
    mainProgram = "udiskie-dmenu";
    platforms = platforms.all;
  };
}
