_final: prev: {
  dmenu = prev.dmenu.overrideAttrs (_oa: {
    src = prev.fetchFromGitHub {
      owner = "bakkeby";
      repo = "dmenu-flexipatch";
      rev = "690436ef27ba260a407b0997eae0eb3bc6af6f71";
      hash = "sha256-j7Px+rtvN63xeiAXXkw8T9g3zGbbDTU2m4ijandX9UA=";
    };

    patches = [./0001-configure-patches.patch];
  });
}
