{
  lib,
  pkgs,
  ...
}:
with pkgs; let
  x11-nim = fetchFromGitHub {
    owner = "nim-lang";
    repo = "x11";
    rev = "29aca5e519ebf5d833f63a6a2769e62ec7bfb83a";
    hash = "sha256-jBNsv8meDvF2ySKewbA+rF2XS+gqydZUl1xhEevD15o=";
  };

  opengl-nim = fetchFromGitHub {
    owner = "nim-lang";
    repo = "opengl";
    rev = "8e2e098f82dc5eefd874488c37b5830233cd18f4";
    hash = "sha256-v3bMDobYQZqX0anBFIUfZx5q5/vxTHO6PDtKQlf5mgU=";
  };
in
  stdenv.mkDerivation rec {
    pname = "boomer";
    name = "boomer";

    src = fetchFromGitHub {
      owner = "tsoding";
      repo = "boomer";
      rev = "3fa84110f2e440ffbd4c462e8cdc241f0b7008be";
      hash = "sha256-If7Qne0xL2rXOu9/17JV6MFFqFXnKUMoP4INptwx6B8=";
    };

    buildInputs = [nim xorg.libX11 xorg.libXrandr libGL makeWrapper];

    buildPhase = ''
      runHook preBuild
      HOME=$TMPDIR
      nim -p:${x11-nim}/ -p:${opengl-nim}/src c -d:release src/boomer.nim
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
