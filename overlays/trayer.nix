_final: prev: {
  trayer = prev.trayer.overrideAttrs (_oa: {
    src = prev.fetchFromGitHub {
      # owner = "sargon";
      # repo = "trayer-srg";
      # rev = "b5693f5c6f554320d5a9bf328170f3194c1de2aa";
      # hash = "sha256-tlYZZzs9cndz094OYhz15gW8jPuc8zSjjz7PKRJT29c=";

      # latest updated fork, adds --iconsize flag
      owner = "theo-coder";
      repo = "trayer-srg";
      rev = "91874ac93af623cba76b03cebfed92bb6f4fb4fc";
      hash = "sha256-jXIWR4vvB7UQnKXlzwnelyV45C0cgTxNtybWv3P5kNo=";
    };
  });
}
