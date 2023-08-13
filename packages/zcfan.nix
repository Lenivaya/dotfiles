{
  lib,
  stdenv,
  fetchFromGitHub,
}:
stdenv.mkDerivation rec {
  pname = "zcfan";
  version = "1.2.1";

  src = fetchFromGitHub {
    owner = "cdown";
    repo = "zcfan";
    rev = version;
    hash = "sha256-XngchR06HP2iExKJVe+XKBDgsv98AEYWOkl1a/Hktgs=";
  };

  makeFlags = ["DESTDIR=${placeholder "out"}" "PREFIX="];

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
