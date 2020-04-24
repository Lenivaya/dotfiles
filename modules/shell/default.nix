{ config, lib, pkgs, ... }:

{
  imports =
    [ ./git.nix ./gnupg.nix ./pass.nix ./ranger.nix ./tmux.nix ./zsh.nix ];
}
