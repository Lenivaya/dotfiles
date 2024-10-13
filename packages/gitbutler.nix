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
stdenv.mkDerivation {
  pname = "git-butler";
  version = "latest";

  src = fetchurl {
    url = "https://releases.gitbutler.com/releases/release/0.13.0-1331/linux/x86_64/git-butler_0.13.0_amd64.deb";
    sha256 = "0bs8vl5wb3af7wvnnsm0wlv535d8ipfwjfa2v97f048hz0kb6pvv";
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
    platforms = [ "x86_64-linux" ];
    license = licenses.mit;
    mainProgram = "git-butler";
  };
}
