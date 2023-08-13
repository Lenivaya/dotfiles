{
  lib,
  pkgs,
}:
with pkgs; let
  x11-nim = fetchFromGitHub {
    owner = "nim-lang";
    repo = "x11";
    rev = "10f1c2f42759839f5e25f32c52e9f01103b64f59";
    hash = "sha256-P8wtdwB3QDHQjgI8h1cumuLIIowSSoI4wQbNWX3P57c=";
  };

  opengl-nim = fetchFromGitHub {
    owner = "nim-lang";
    repo = "opengl";
    rev = "84bd93b2440b9a8144d7430d23d6c0496737975d";
    hash = "sha256-OHdoPJsmCFdyKV7FGd/r+6nh6NRF7TPhuAx7o/VpiDg=";
  };
in
  clangStdenv.mkDerivation rec {
    pname = "boomer";
    name = "boomer";

    src = fetchFromGitHub {
      owner = "tsoding";
      repo = "boomer";
      rev = "fa6660fe44e0f76825328923d5b1238306081026";
      hash = "sha256-lm1rHa7y7XZ055TBJxg2UXswJeG9q1JfEAWcEvuZVYQ=";
    };

    buildInputs = [nim xorg.libX11 xorg.libXrandr libGL makeWrapper];

    buildPhase = ''
      runHook preBuild
      HOME=$TMPDIR
      nim -p:${x11-nim}/ -p:${opengl-nim}/src c -d:release --cc:clang --passL:-flto src/boomer.nim
      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall
      install -Dt $out/bin src/boomer
      runHook postInstall
    '';

    fixupPhase = let
      libPath =
        lib.makeLibraryPath [stdenv.cc.cc xorg.libX11 xorg.libXrandr libGL];
    in ''
      runHook preFixup
      patchelf --set-rpath ${libPath} $out/bin/boomer
      wrapProgram "$out/bin/boomer" --set LIBGL_ALWAYS_SOFTWARE 1
      runHook postFixup
    '';

    meta = with lib; {
      license = licenses.mit;
      platforms = platforms.linux;
      homepage = "https://github.com/tsoding/boomer";
    };
  }
