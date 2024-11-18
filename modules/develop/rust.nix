{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.dev.rust;
in
{
  options.modules.dev.rust.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      clang
      llvmPackages.bintools

      # rustc
      # cargo
      rustup

      # clippy
      # rustfmt

      rust-analyzer
      cargo-edit

      # oftern required
      openssl
      pkg-config
    ];

    env = {
      CARGO_HOME = "$XDG_DATA_HOME/cargo";
      RUSTUP_HOME = "$XDG_DATA_HOME/.rustup";
      PATH = [ "$CARGO_HOME/bin" ];
    };
  };
}
