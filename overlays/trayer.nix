_final: prev: {
  trayer = prev.trayer.overrideAttrs (_oa: {
    src = prev.fetchFromGitHub {
      owner = "sargon";
      repo = "trayer-srg";
      rev = "b5693f5c6f554320d5a9bf328170f3194c1de2aa";
      hash = "sha256-tlYZZzs9cndz094OYhz15gW8jPuc8zSjjz7PKRJT29c=";
    };
  });
}
