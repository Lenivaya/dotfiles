self: super: {
  haskellPackages = super.haskellPackages.override {
    overrides = hself: hsuper: {
      xmonad = self.xmonad_0_17_0;
      xmonad-contrib = self.xmonad-contrib_0_17_0;
    };
  };
}
