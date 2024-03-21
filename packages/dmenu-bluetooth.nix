{
  lib,
  stdenv,
  fetchFromGitHub,
  bluez,
  dmenu,
  makeWrapper,
  ...
}:
stdenv.mkDerivation rec {
  pname = "dmenu-bluetooth";
  version = "unstable-2023-07-15";

  src = fetchFromGitHub {
    owner = "Layerex";
    repo = "dmenu-bluetooth";
    rev = "96e2e3e1dd7ea2d2ab0c20bf21746aba8d70cc46";
    hash = "sha256-0G2PXWq9/JsLHnbOIJWSWWqfnBgOxaA8N2VyCbTUGmI=";
  };

  nativeBuildInputs = [makeWrapper];

  prePatch = ''
    substituteInPlace ./dmenu-bluetooth \
      --replace 'DMENU_BLUETOOTH_LAUNCHER -i' DMENU_BLUETOOTH_LAUNCHER
  '';

  installPhase = ''
    runHook preInstall

    install -D --target-directory=$out/bin/ ./dmenu-bluetooth

    wrapProgram $out/bin/dmenu-bluetooth \
      --prefix PATH ":" ${lib.makeBinPath [bluez dmenu]}

    runHook postInstall
  '';

  meta = with lib; {
    description = "A script that generates a dmenu (or other) menu that uses bluetoothctl to connect to bluetooth devices and display status info";
    homepage = "https://github.com/Layerex/dmenu-bluetooth";
    license = licenses.gpl3Only;
    maintainers = with maintainers; [];
    mainProgram = "dmenu-bluetooth";
    platforms = platforms.all;
  };
}
