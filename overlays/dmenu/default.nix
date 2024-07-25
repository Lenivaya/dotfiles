_final: prev: {
  dmenu = prev.dmenu.overrideAttrs (_oa: {
    src = prev.fetchFromGitHub {
      owner = "bakkeby";
      repo = "dmenu-flexipatch";
      rev = "76549d014ef6abdecb1d676e5d4de160f86ba2d9";
      hash = "sha256-bZbMGA5w3gLMf/kOzy+ji8GNkGZ3Srt6TfKwK6BLc4A=";
    };

    patches = [ ./0001-configure-patches.patch ];
  });
}
