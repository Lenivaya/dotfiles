{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
  ...
}:
stdenvNoCC.mkDerivation {
  pname = "fast-font";
  version = "final-unstable";

  src = fetchFromGitHub {
    owner = "Born2Root";
    repo = "Fast-Font";
    rev = "be82e4c72cc3f40d44a270b2d4d7d45a79d676ee";
    hash = "sha256-ZSxt1BDmsbvz2FVMpvgybvba0YEraz1SDo8tFNFJuhs=";
  };

  installPhase = ''
    runHook preInstall

    install -Dm644 *.ttf -t $out/share/fonts/truetype

    runHook postInstall
  '';

  meta = with lib; {
    homepage = "https://github.com/Born2Root/Fast-Font";
    description = "A font to help you read faster.";
    license = licenses.mit;
    maintainers = [ ];
  };
}
