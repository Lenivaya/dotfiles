{ stdenv, fetchgit, writeTextFile }:

let
  config_file = writeTextFile {
    name = "abstract_ring.plymouth";
    text = ''
      [Plymouth Theme]
      Name=abstract_ring
      Description=display abstract ring
      Comment=created By Aditya Shakya (@adi1090x)
      ModuleName=script

      [script]
      ImageDir=etc/plymouth/themes/abstract_ring
      ScriptFile=etc/plymouth/themes/abstract_ring/abstract_ring.script
    '';
  };
in stdenv.mkDerivation rec {
  name = "abstract_ring";

  src = fetchGit { url = "https://github.com/adi1090x/plymouth-themes"; };

  buildInputs = [ stdenv ];

  configurePhase = ''
    install_path=$out/share/plymouth/themes/
    mkdir -p $install_path
  '';

  buildPhase = ''
    substitute ${config_file} "pack_1/abstract_ring/abstract_ring.plymouth"
  '';

  installPhase = ''
    cd pack_1 && cp -r abstract_ring $install_path
  '';

  meta = with stdenv.lib; { platfotms = platforms.linux; };
}
