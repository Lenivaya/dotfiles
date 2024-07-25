{
  pkgs ? import <nixpkgs> { },
  preCommitHook ? "",
}:
with pkgs;
mkShell {
  buildInputs = [
    git
    gnupg
    nix-zsh-completions
    oxipng
  ];
  shellHook = ''
    export DOTFILES="$(pwd)"
    ${preCommitHook}
  '';
}
