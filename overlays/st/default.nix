_final: prev: {
  st =
    (prev.st.overrideAttrs (_oa: {
      src = prev.fetchFromGitHub {
        owner = "bakkeby";
        repo = "st-flexipatch";
        rev = "1c03f10db9d52c3b9417eeff2e81d0a370280834";
        hash = "sha256-W1kHL7FsA9V+EYz7NgFpnGucEQ31CgM4hChzcVxSA2o=";
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
