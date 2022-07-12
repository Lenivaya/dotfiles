{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.dev.elixir;
in {
  options.modules.dev.elixir.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      erlang
      elixir
      elixir_ls
    ];
  };
}
