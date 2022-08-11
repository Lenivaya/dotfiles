 self: super: {
   spotify = super.callPackage ./spotify.nix {
     curl = super.curl.override {
       gnutlsSupport = true;
       opensslSupport = false;
     };
     adblock = super.callPackage ./adblock.nix {};
   };
 }
