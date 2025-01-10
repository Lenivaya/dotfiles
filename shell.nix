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
        nixfmt-rfc-style = {
          enable = true;
        };
        deadnix = {
          enable = true;
          settings = {
            edit = true;
          };
        };
        fourmolu.enable = true;
        # statix.enable = true;
        # convco.enable = true;
        # typos = {
        #   enable = true;
        #   types = [ "text" ];
        # };
      };
    }).shellHook;
in
mkShell {
  buildInputs = [
    git
    gnupg
    just
    nix-zsh-completions
  ];
  shellHook = ''
    export DOTFILES="$(pwd)"
    ${preCommitHook}
  '';
}
