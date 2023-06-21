{
  lib,
  stdenvNoCC,
  fetchzip,
}:
stdenvNoCC.mkDerivation rec {
  pname = "pragmasevka";
  version = "1.6.4";

  src = fetchzip {
    url = "https://github.com/shytikov/pragmasevka/releases/download/v${version}/Pragmasevka.zip";
    stripRoot = false;
    hash = "sha256-9ZBozMYjNy/9IO+2Rl6SQC9h+aLBYfG7K4q6GKIOKeo=";
  };

  installPhase = ''
    runHook preInstall

    install -Dm644 *.ttf -t $out/share/fonts/truetype

    runHook postInstall
  '';

  meta = with lib; {
    description = "PragmataPro meets Iosevka";
    homepage = "https://github.com/shytikov/pragmasevka";
    license = licenses.ofl;
    maintainers = [maintainers.jboyens];
    platforms = platforms.all;
  };
}
