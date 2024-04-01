{
  # appimageTools,
  stdenv,
  lib,
  pkgs,
  fetchurl,
  ...
}:
# appimage
# works but browser can't be opened
# let
#   name = "gitbutler";
#   src = inputs.gitbutler;
# in
#   appimageTools.wrapType2 rec {
#     inherit name src;
#     extraPkgs = pkgs:
#       with pkgs; [
#         libsoup
#         webkitgtk
#         glib-networking
#         libthai
#       ];
#     passthru.appimage_content = appimageTools.extract {inherit name src;};
#   }
#
# https://github.com/PHSix/nix-config/blob/42c96b5e45cdb9aefb2a055ea2826259aad5faeb/pkgs/git-butler.nix
stdenv.mkDerivation rec {
  pname = "git-butler";
  version = "0.10.28";

  src = fetchurl {
    # url = "https://releases.gitbutler.com/releases/release/${version}-713/linux/x86_64/git-butler_${version}_amd64.deb";
    url = "https://releases.gitbutler.com/releases/release/0.10.28-764/linux/x86_64/git-butler_0.10.28_amd64.deb";
    sha256 = "sha256-J2qLrYnfeEgdu5CC+bsWfu1HUpT6M3wCZ62FpduDIbo=";
  };

  nativeBuildInputs = with pkgs; [
    dpkg
    wrapGAppsHook
    autoPatchelfHook
    tree
  ];

  buildInputs = with pkgs; [
    openssl
    webkitgtk
    glib-networking
    libsoup
    libthai
    stdenv.cc.cc
  ];

  installPhase = ''
    runHook preInstall
    tree

    mkdir -p $out
    mv usr/* $out

    runHook postInstall
  '';

  meta = with lib; {
    description = "A Git client for simultaneous branches on top of your existing workflow.";
    homepage = "https://gitbutler.com/";
    platforms = ["x86_64-linux"];
    license = licenses.mit;
    mainProgram = "git-butler";
  };
}
