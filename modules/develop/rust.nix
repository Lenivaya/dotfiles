{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.dev.rust;
in {
  options.modules.dev.rust.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    user.packages = with pkgs; [rustup rustfmt rust-analyzer rls];

    env.RUSTUP_HOME = "$XDG_DATA_HOME/.rustup";
    env.CARGO_HOME = "$XDG_DATA_HOME/.cargo";
    env.PATH = ["$CARGO_HOME/bin"];
  };
}
