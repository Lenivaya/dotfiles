{ lib, buildPythonPackage, fetchPypi, }:

buildPythonPackage rec {
  pname = "my_cookies";
  version = "0.1.1";

  src = fetchPypi { inherit pname version; };

  meta = with lib; {
    description =
      "Package for retrieving leetcode cookies from Chrome with local keyring.";
    platforms = platforms.unix;
  };
}
