_final: prev: {
  xob = prev.xob.overrideAttrs (oa: {
    src = prev.fetchFromGitHub {
      owner = "juster-0";
      repo = "xob";
      rev = "66d13ac4e35c705b802eea4628725831a2965c28";
      hash = "sha256-gnb7Yg/tKGDVDtp6xNyolnIfZ8e31HNQJ8os2YAegGw=";
    };

    buildInputs =
      oa.buildInputs
      ++ (with prev.xorg; [
        libXft
        libXext
        libXrandr
      ]);

    # makeFlags = oa.makeFlags ++ ["enable_alpha=no"];
  });
}
