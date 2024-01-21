_final: prev: {
  skippy-xd = prev.skippy-xd.overrideAttrs (_oa: {
    src = prev.fetchFromGitHub {
      owner = "felixfung";
      repo = "skippy-xd";
      rev = "4f2cb0b957fb2be446fae759a576ed871c68747d";
      hash = "sha256-9QdlVt2CLd5tnPyIGw8PIURGaymdLgkTtWFXKzJe1H4=";
    };

    # src = prev.fetchFromGitHub {
    #   owner = "felixfung";
    #   repo = "skippy-xd";
    #   rev = "9ffbaaddb7f91a171067980b1abd518b3c09eb74";
    #   hash = "sha256-4ehoUqT6NUGQMdHJv/KDTqbO+tSbK9UjHvV0zUrUCwE=";
    # };
  });
}
