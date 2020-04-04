{ config, options, pkgs, lib, ... }:
with lib; {
  options.modules.services.ssh = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf config.modules.services.ssh.enable {
    services.openssh = {
      enable = true;
      forwardX11 = true;
      permitRootLogin = "no";
      passwordAuthentication = true;

      # Allow local LAN to connect with passwords
      extraConfig = ''
        Match address 192.168.0.0/24
        PasswordAuthentication yes
      '';
    };

    users.users.leniviy.openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDXRowxPRtucIfNFHhd950gZnmHf4fxJuL1UalgSgpctDQ8JwkxxZs93u7ad+bcVGQV8oTRW5czMNz5V8Ary0bQkcQqbOB0AvrBlqQRl/HnluoscITpT/wwXs6Rp+psvJbrTHo9wwhjpku9ITugqcikLzYYA8rU9AwyfkeTmHTbq6UdAk2fnW5mu3S8XhJSJOdZGrOGp+QmEAaSzUMl2/6SSdGhUblJoxaGkEesXR+7pYN6EhtjghD3lb/PUCsyU4UaGl+69lodxE28pmvVIO2thULG6MpgDaxmgsfaNRhUWoD/3vGclOTFDlmyA6c9cMVZjC4Lf9h9ZJbGZKxmZ8xt xocada@gmail.com"
    ];
  };
}
