{ config, lib, pkgs, ... }:

{
  imports = [ ./git.nix ./gnupg.nix ./ranger.nix ./tmux.nix ./zsh.nix ];
}
