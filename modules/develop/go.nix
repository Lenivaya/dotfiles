{ pkgs, ... }: {
  my.home.programs.go = {
    enable = true;
    package = pkgs.go;
    goBin = "go/bin";
    goPath = "go";
  };

  my.packages = with pkgs; [ goimports ];
}
