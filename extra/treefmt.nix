{ treefmt-wrapper }:
treefmt-wrapper {
  projectRootFile = "flake.nix";

  programs = {
    nixfmt.enable = true;
    deadnix.enable = true;
    shfmt.enable = true;
    ruff-format.enable = true;
    rufo.enable = true;
    mdsh.enable = true;
    yamlfmt.enable = true;
    prettier.enable = true;
    toml-sort.enable = true;
    # fourmolu.enable = true;
  };
}
