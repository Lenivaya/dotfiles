{
  allowBroken = false;
  allowUnfree = true; 

  packageOverrides = pkgs: {
    nur = import (builtins.fetchTarball {
      url = "https://github.com/nix-community/NUR/archive/3a6a6f4da737da41e27922ce2cfacf68a109ebce.tar.gz";
      sha256 = "04387gzgl8y555b3lkz9aiw9xsldfg4zmzp930m62qw8zbrvrshd";
  }) {};

    unstable = import (builtins.fetchTarball {
      url = "https://github.com/nixos/nixpkgs-channels/archive/b0bbacb52134a7e731e549f4c0a7a2a39ca6b481.tar.gz";
      sha256 = "15ix4spjpdm6wni28camzjsmhz0gzk3cxhpsk035952plwdxhb67";
  }) {};

  };
}
