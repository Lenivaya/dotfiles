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
    url = "https://releases.gitbutler.com/releases/release/0.14.6-1650/linux/x86_64/GitButler_0.14.6_amd64.deb";
    sha256 = "1j977smg04ysid8gqrqwgpf39s9xa37czdqlsl69xha88f4cfya0";
  };

  nativeBuildInputs = with pkgs; [
    dpkg
    wrapGAppsHook
    autoPatchelfHook
    tree
  ];

  buildInputs = with pkgs; [
    glib-networking
    libthai
    stdenv.cc.cc
    gtk3
    glib
    dbus
    openssl_3
    librsvg
    gettext
    libiconv
    libsoup
    libsoup_3
    webkitgtk
    webkitgtk_4_1
    cairo
    pango
    harfbuzz
    gdk-pixbuf
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
