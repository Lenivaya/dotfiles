{
  lib,
  pkgs,
  ...
}:
with builtins;
with lib; rec {
  optimizeWithFlags = pkg: flags:
    pkgs.lib.overrideDerivation pkg (old: let
      newflags = pkgs.lib.foldl' (acc: x: "${acc} ${x}") "" flags;
      oldflags =
        if (pkgs.lib.hasAttr "NIX_CFLAGS_COMPILE" old)
        then "${old.NIX_CFLAGS_COMPILE}"
        else "";
    in {NIX_CFLAGS_COMPILE = "${oldflags} ${newflags}";});

  withClang = pkg:
    pkgs.lib.overrideDerivation pkg (_oa: {
      stdenv = pkgs.clangStdenv;
    });

  withFastGCC = pkg:
    pkgs.lib.overrideDerivation pkg (_oa: {
      stdenv = pkgs.fastStdenv;
    });

  withThinLTO = pkg:
    optimizeWithFlags pkg ["-flto=thin"];

  optimizeForThisHost = pkg:
    optimizeWithFlags pkg ["-O3" "-march=native" "-fPIC"];

  withDebuggingCompiled = pkg: optimizeWithFlags pkg ["-DDEBUG"];
}
