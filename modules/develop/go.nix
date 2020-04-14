{ pkgs, ... }: {
  my.env = { GOROOT = [ "${pkgs.unstable.go.out}/share/go" ]; };

  my.home.programs.go = {
    enable = true;
    package = pkgs.unstable.go;
    goBin = "go/bin";
    goPath = "go";
  };

  my.packages = with pkgs.unstable; [ goimports ];

}
