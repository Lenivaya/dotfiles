{ stdenv, requireFile, unzip, pkgs }:

stdenv.mkDerivation rec {
  name = "PragmataPro${version}";
  version = "0.828";

  src = requireFile {
    url = "file:///etc/nixos/modules/fonts/${name}.zip";
    sha256 = "7049462b4a8122300f661c1c5a393b558a23c061b18e6afe5d2f77c16a63f71e";
  };

  buildInputs = [ unzip ];

  configurePhase = ''
    install_path=$out/share/fonts/truetype/pragmatapro
    mkdir -p $install_path
  '';

  buildPhase = ''
    unzip $src
  '';

  installPhase = ''
    find -name "PragmataPro*.ttf" -exec mv {} $install_path \;
  '';

  meta = with stdenv.lib; {
    platfotms = platforms.linux;
  };
}
