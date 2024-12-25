_final: prev: {
  zcfan = prev.zcfan.overrideAttrs (_oa: {
    src = prev.fetchFromGitHub {
      owner = "cdown";
      repo = "zcfan";
      rev = "2c9dd133c597b43fee2819e353ba5cd38e3d369a";
      hash = "sha256-/q9jDqjG4g211CTb4ahagpxux2BsZWTEyoAY8kRRTB8=";
    };
  });
}
