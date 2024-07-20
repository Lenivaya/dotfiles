{
  lib,
  stdenv,
  fetchFromGitHub,
  ...
}:
stdenv.mkDerivation rec {
  pname = "zcfan";
  version = "master";

  src = fetchFromGitHub {
    owner = "cdown";
    repo = "zcfan";
    rev = version;
    hash = "sha256-wfbs/FdsOlDSHC5P8/5LhdM2eEr0wJkmeom6vjntjCQ=";
  };

  makeFlags = [
    "DESTDIR=${placeholder "out"}"
    "PREFIX="
  ];

  postInstall = ''
    mkdir $out/bin
    ln -sf $out/usr/local/share $out/share
    ln -sf $out/usr/local/bin/zcfan $out/bin/zcfan
  '';

  meta = with lib; {
    description = "A zero-configuration fan daemon for ThinkPads";
    homepage = "https://github.com/cdown/zcfan";
    license = licenses.mit;
    maintainers = with maintainers; [];
  };
}
