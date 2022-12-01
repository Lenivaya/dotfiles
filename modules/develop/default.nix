{
  config,
  options,
  lib,
  pkgs,
  ...
}: {
  config = {
    user.packages = with pkgs; [
      # Nix
      nixfmt
      niv
      # nixpkgs-fmt
      alejandra
      statix
    ];
  };
}
