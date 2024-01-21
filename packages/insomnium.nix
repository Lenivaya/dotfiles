{
  pkgs,
  lib,
  ...
}: let
  pname = "insomnium";
  version = "0.2.3-a";
  name = "${pname}-${version}";

  src = pkgs.fetchurl {
    url = "https://github.com/ArchGPT/insomnium/releases/download/core%400.2.3-a/Insomnium.Core-0.2.3-a.AppImage";
    sha256 = "0wd1ly4lm43kipgj5sz64fkm4cn2zl67jkz33w7x81m83hbl0nl4";
  };
in
  pkgs.appimageTools.wrapType1 rec {
    # ISO 9660 file that are also ELF executables.
    inherit name src;

    extraInstallCommands = ''
      mv $out/bin/${name} $out/bin/${pname}
    '';

    meta = with lib; {
      description = "Fast local API testing tool that is privacy-focused and 100% local";
      homepage = "https://github.com/ArchGPT/insomnium";
      license = licenses.mit;
      maintainers = [];
    };
  }
