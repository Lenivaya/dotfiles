{ stdenv, pkgs, ... }:
with stdenv;
with pkgs;
rustPlatform.buildRustPackage rec {
  pname = "caffeinate";
  version = "master";

  src = fetchFromGitHub {
    owner = "rschmukler";
    repo = "caffeinate";
    rev = version;
    hash = "sha256-22gQ+rXANrCgaqtji3BAA0ITmEoVua+q4C+KEn5LM90=";
  };

  cargoHash = "sha256-wz1CXmlqKBa+TlzsWZKZN73RmFHQGsvuwBHTATun31s=";

  meta = {
    description = "Keeping xidlehook woke";
    homepage = "https://github.com/rschmukler/caffeinate";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [ ];
    mainProgram = "caffeinate";
  };
}
