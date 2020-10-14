{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.cc;
in {
  options.modules.dev.cc = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      clang
      gcc
      bear
      gdb
      cmake
      llvmPackages.libcxx
      ccls
      rtags
      llvm
    ];
  };
}
