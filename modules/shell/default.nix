{ config, lib, pkgs, ... }:

{
  imports = [
    ./git.nix
    ./gnupg.nix
    ./pass.nix
    ./direnv.nix
    ./ranger.nix
    ./tmux.nix
    ./zsh.nix
  ];
}
