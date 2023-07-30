_final: prev: {
  avizo = prev.avizo.overrideAttrs (oa: {
    src = prev.fetchFromGitHub {
      owner = "misterdanb";
      repo = "avizo";
      rev = "9cd5319503584e2e7e0e1ec1fe35c5b8c3e7262e";
      hash = "sha256-fW4y6Ml63XS0uVF6dBDOL9uKkoHE3L3ylwiHs6immrk=";
    };

    # Making it centered on X11 and fixing some bugs
    patches =
      (oa.patches or [])
      ++ [./0001-fixes-for-positions-and-misc-things.patch];
  });
}
