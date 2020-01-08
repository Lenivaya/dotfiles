{ pkgs, ... }:

{
  home.sessionVariables = { GOROOT = [ "${pkgs.unstable.go.out}/share/go" ]; };

  programs.go = {
    enable = true;
    package = pkgs.unstable.go;
    goBin = "go/bin";
    goPath = "go";
  };

  # For writting in emacs
  programs.go.packages = {
    "github.com/motemen/gore/cmd/gore" = builtins.fetchGit "https://github.com/motemen/gore";
    "github.com/mdempsky/gocode" = builtins.fetchGit "https://go.googlesource.com/tools";
    "golang.org/x/tools/cmd/godoc" = builtins.fetchGit "https://go.googlesource.com/tools";
    "golang.org/x/tools/cmd/goimports" = builtins.fetchGit "https://go.googlesource.com/tools";
    "golang.org/x/tools/cmd/gorename" = builtins.fetchGit "https://go.googlesource.com/tools";
    "golang.org/x/tools/cmd/guru" = builtins.fetchGit "https://go.googlesource.com/tools";
    "github.com/cweill/gotests/..." = builtins.fetchGit "https://github.com/cweill/gotests";
    "github.com/fatih/gomodifytags" = builtins.fetchGit "https://github.com/fatih/gomodifytags";
  };
}
