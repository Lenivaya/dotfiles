_final: prev: {
  zcfan = prev.zcfan.overrideAttrs (_oa: {
    src = prev.fetchFromGitHub {
      owner = "cdown";
      repo = "zcfan";
      rev = "ae6081f34ebe2d6d10556e726c2b5750384ad131";
      hash = "sha256-XrjMhguPlQAjfWYY1WYj8ROZn8U6LXmzxskqlvE5SAU=";
    };
  });
}
