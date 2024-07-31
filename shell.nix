{
  pkgs ? import <nixpkgs> { },
  inputs,
  system,
}:
with pkgs;
let
  preCommitHook =
    (inputs.pre-commit-hooks.lib.${system}.run {
      src = ./.;
      hooks = {
        editorconfig-checker.enable = true;
        black.enable = true;
        prettier.enable = true;
        shellcheck.enable = true;
        shfmt.enable = true;
        # yamllint.enable = true;
        actionlint.enable = true;
        nixfmt = {
          enable = true;
          package = pkgs.nixfmt-rfc-style;
        };
        deadnix = {
          enable = true;
          settings = {
            edit = true;
          };
        };
        # statix.enable = true;
        # convco.enable = true;
        fourmolu.enable = true;
        typos = {
          enable = true;
          types = [ "text" ];
        };
      };
    }).shellHook;
in
mkShell {
  buildInputs = [
    git
    gnupg
    just
    nix-zsh-completions
    oxipng
  ];
  shellHook = ''
    export DOTFILES="$(pwd)"
    ${preCommitHook}
  '';
}
