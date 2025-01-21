{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
  ...
}:
stdenvNoCC.mkDerivation {
  pname = "fast-font";
  version = "final-unstable-2024-12-15";

  src = fetchFromGitHub {
    owner = "Born2Root";
    repo = "Fast-Font";
    rev = "f6b5d706ac1c9266ccfee5a5c08599bcc9f4f328";
    hash = "sha256-TDIjEf9deASNL0KoWtlj/dZM6zRpW3JO1W5fVwz5BDI=";
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
