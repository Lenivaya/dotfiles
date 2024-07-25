{ stdenv, pkgs, ... }:
with stdenv;
with pkgs;
rustPlatform.buildRustPackage rec {
  pname = "caffeinate";
  version = "0.1.0";

  src = fetchFromGitHub {
    owner = "rschmukler";
    repo = pname;
    rev = "e20985a4b630eb5c76e16c2547da0aba65f097d5";
    sha256 = "1p9k9dz152igw2mazf8m9ac16hh381q8nqxbdahb0dn0npx10s6v";
  };

  cargoSha256 = "tZ3XVrlIs3xZvPiTU4fchus2lgenfVPB+xz0PhVu9gg=";

  meta = with stdenv.lib; {
    description = "A command-line app bringing caffeinate functionality to xidlehook.";
    homepage = "https://github.com/rschmukler/caffeinate";
  };
}
