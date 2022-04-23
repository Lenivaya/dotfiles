# self: super: {
#   haskellPackages = super.haskellPackages.override (old: {
#     overrides = self.lib.composeExtensions (old.overrides or (_: _: { }))
#       (hself: hsuper: {
#         xmonad = hsuper.xmonad_0_17_0;
#         xmonad-contrib = hsuper.xmonad-contrib_0_17_0;
#         xmonad-extras = hsuper.xmonad-extras_0_17_0;
#       });
#   });
# }

self: super: {
  xmonad = self.xmonad_0_17_0;
  xmonad-contrib = self.xmonad_0_17_0;
  xmonad-extras = self.xmonad-extras_0_17_0;
}
