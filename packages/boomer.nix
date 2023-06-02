{
  lib,
  pkgs,
}:
with pkgs; let
  x11-nim = fetchgit {
    url = "https://github.com/nim-lang/x11";
    rev = "2093a4c01360cbb5dd33ab79fd4056e148b53ca1";
    sha256 = "sha256-2XRyXiBxAc9Zx/w0zRBHRZ240qww0FJvIvOKZ8YH50A=";
  };

  opengl-nim = fetchgit {
    url = "https://github.com/nim-lang/opengl";
    rev = "640c1ced89e4f80ad15985f7423ab2096c969e89";
    sha256 = "sha256-3kyxeZHiNlOHJMTQK5lWaMfEktI+cjPbDsUU9KtsyCM=";
  };
in
  clangStdenv.mkDerivation rec {
    pname = "boomer";
    version = "fa6660fe44e0f76825328923d5b1238306081026";

    src = fetchgit {
      url = "https://github.com/tsoding/boomer";
      rev = version;
      sha256 = "sha256-lm1rHa7y7XZ055TBJxg2UXswJeG9q1JfEAWcEvuZVYQ=";
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
      libPath = lib.makeLibraryPath [stdenv.cc.cc xorg.libX11 xorg.libXrandr libGL];
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
