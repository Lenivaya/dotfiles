{
  fetchurl,
  lib,
  adblock,
  stdenv,
  spicetify-cli,
  squashfsTools,
  xorg,
  alsa-lib,
  makeWrapper,
  wrapGAppsHook,
  dpkg,
  freetype,
  glib,
  pango,
  cairo,
  atk,
  gdk-pixbuf,
  gtk3,
  cups,
  nspr,
  nss,
  libpng,
  libnotify,
  libgcrypt,
  systemd,
  fontconfig,
  dbus,
  expat,
  ffmpeg,
  curl,
  zlib,
  gnome,
  at-spi2-atk,
  at-spi2-core,
  libpulseaudio,
  libdrm,
  mesa,
  libxkbcommon,
}: let
  version = "1.1.84.716";
  commit = "gc5f8b819";

  rev = "60";

  deps = [
    alsa-lib
    at-spi2-atk
    at-spi2-core
    atk
    cairo
    cups
    curl
    dbus
    expat
    ffmpeg
    fontconfig
    freetype
    gdk-pixbuf
    glib
    gtk3
    libdrm
    libgcrypt
    libnotify
    libpng
    libpulseaudio
    libxkbcommon
    mesa
    nss
    pango
    stdenv.cc.cc
    systemd
    xorg.libICE
    xorg.libSM
    xorg.libX11
    xorg.libxcb
    xorg.libXcomposite
    xorg.libXcursor
    xorg.libXdamage
    xorg.libXext
    xorg.libXfixes
    xorg.libXi
    xorg.libXrandr
    xorg.libXrender
    xorg.libXScrnSaver
    xorg.libxshmfence
    xorg.libXtst
    zlib
  ];
in
  stdenv.mkDerivation {
    pname = "spotify-unwrapped";
    inherit version;

    src = fetchurl {
      url = "http://repository.spotify.com/pool/non-free/s/spotify-client/spotify-client_${version}.${commit}_amd64.deb";
      sha512 = "sha512-PMJfKK55GsJmBxF6XfZo+APtjljwrOCFAQpiQv3el3Zr3Bx1JWCFB5XJtDJPPgGZN/6a8niKGUbrtw7ngfUNmQ==";
    };

    nativeBuildInputs = [makeWrapper wrapGAppsHook squashfsTools];
    buildInputs = [dpkg spicetify-cli];

    dontStrip = true;
    dontPatchELF = true;

    unpackPhase = ''
      mkdir -p $out
      dpkg -x $src $out
      cp -av $out/usr/* $out
      rm -rf $out/opt $out/usr
    '';

    # Prevent double wrapping
    dontWrapGApps = true;

    installPhase = ''
      runHook preInstall

      libdir=$out/lib/spotify
      mkdir -p $libdir

      ln -s ${nspr.out}/lib/libnspr4.so $libdir/libnspr4.so
      ln -s ${nspr.out}/lib/libplc4.so $libdir/libplc4.so

      ln -s ${ffmpeg.out}/lib/libavcodec.so* $libdir
      ln -s ${ffmpeg.out}/lib/libavformat.so* $libdir

      rpath="$out/share/spotify:$libdir"

      patchelf \
        --interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
        --set-rpath $rpath $out/share/spotify/spotify

      librarypath="${lib.makeLibraryPath deps}:$libdir"
      wrapProgram $out/share/spotify/spotify \
        ''${gappsWrapperArgs[@]} \
        --prefix LD_LIBRARY_PATH : "$librarypath" \
        --prefix PATH : "${gnome.zenity}/bin"

      # fix Icon line in the desktop file (#48062)
      sed -i "s:^Icon=.*:Icon=spotify-client:" "$out/share/spotify/spotify.desktop"

      # Desktop file
      mkdir -p "$out/share/applications/"
      cp "$out/share/spotify/spotify.desktop" "$out/share/applications/"

      substituteInPlace "$out/share/applications/spotify.desktop" \
        --replace 'Exec=spotify %U' 'Exec=env LD_PRELOAD=${adblock}/usr/local/lib/spotify-adblock.so spotify %U'

      # Icons
      for i in 16 22 24 32 48 64 128 256 512; do
        ixi="$i"x"$i"
        mkdir -p "$out/share/icons/hicolor/$ixi/apps"
        ln -s "$out/share/spotify/icons/spotify-linux-$i.png" \
          "$out/share/icons/hicolor/$ixi/apps/spotify-client.png"
      done

      runHook postInstall
    '';
  }
