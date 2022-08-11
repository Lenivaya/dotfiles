{
  rustPlatform,
  fetchFromGitHub,
}:
rustPlatform.buildRustPackage rec {
  pname = "spotify-adblock";
  version = "1.0.2";

  src = fetchFromGitHub {
    owner = "abba23";
    repo = pname;
    rev = "v${version}";
    sha256 = "sha256-YGD3ymBZ2yT3vrcPRS9YXcljGNczJ1vCvAXz/k16r9Y=";
  };

  cargoSha256 = "sha256-bYqkCooBfGeHZHl2/9Om+0qbudyOCzpvwMhy8QCsPRE=";

  postInstall = ''
    mkdir -p $out/usr/local/lib
    mkdir -p $out/etc/spotify-adblock

    mv $out/lib/libspotifyadblock.so $out/usr/local/lib/spotify-adblock.so
    mv config.toml $out/etc/spotify-adblock
  '';
}
