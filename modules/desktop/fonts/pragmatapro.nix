{ stdenv, requireFile, unzip, pkgs }:

stdenv.mkDerivation rec {
  name = "PragmataPro${version}";
  version = "0.828";

  src = requireFile {
    url = "file:///etc/dotfiles/modules/desktop/fonts/${name}.zip";
    sha256 = "07ppcdmc2xrgbpz6m3mic70272jm7cwml70wcq7k08l198mlcjbh";
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

  meta = with stdenv.lib; { platfotms = platforms.linux; };
}
