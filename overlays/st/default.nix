_final: prev: {
  st =
    (prev.st.overrideAttrs (_oa: {
      src = prev.fetchFromGitHub {
        owner = "bakkeby";
        repo = "st-flexipatch";
        rev = "f097dbd0798d36f817b29f9881e1989f8864843d";
        hash = "sha256-/icnipnpK5Yi0IgSkVWkP4TRMB9gV1p6m/c+mbmkewo=";
      };

      patches = [./0001-configure-patches.patch];
    }))
    .override {
      extraLibs = with prev; [
        harfbuzz
        libsixel
        xorg.libXcursor
      ];
    };
}
