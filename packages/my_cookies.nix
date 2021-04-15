{ lib, pkgs }:

with pkgs.python3Packages;

buildPythonPackage rec {
  pname = "my_cookies";
  version = "0.1.1";

  src = fetchPypi {
    inherit pname version;
    sha256 = "SDXfJoMCR+cPXt/SkTVjkddpY3dQ4/WjKtwlxUG727A=";
  };

  propagatedBuildInputs = [ browser-cookie3 ];

  meta = with lib; {
    description = "Package for retrieving leetcode cookies from browser";
    platforms = platforms.unix;
  };
}
