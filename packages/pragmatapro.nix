{
  stdenv,
  requireFile,
  unzip,
}:
stdenv.mkDerivation rec {
  name = "PragmataPro${version}";
  version = "0.829";
  buildInputs = [unzip];

  src = requireFile {
    url = "file://path/to/${name}.zip";
    sha256 = "0k2l18lzwqmyhv5g55pd3rwiia8gbkvdngjhbb867ilm89ad2fq9";
  };

  # Work around the "unpacker appears to have produced no directories"
  # case that happens when the archive doesn't have a subdirectory.
  setSourceRoot = "sourceRoot=`pwd`";

  installPhase = ''
    # unzip $src

    install_path=$out/share/fonts/truetype/pragmatapro
    mkdir -p $install_path

    find -name "PragmataPro*.ttf" -exec mv {} $install_path \;
  '';

  meta = with stdenv.lib; {platfotms = platforms.linux;};
}
