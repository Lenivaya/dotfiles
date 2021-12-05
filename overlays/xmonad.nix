(self: super: {
  haskellPackages = super.haskellPackages.override {
    overrides = hself: hsuper: {
      # xmonad-contrib = hsuper.xmonad-contrib_0_17_0;
      # xmonad = hsuper.xmonad_0_17_0;
      # xmonad-extras = hsuper.xmonad-extras_0_17_0;
    };
  };
})
