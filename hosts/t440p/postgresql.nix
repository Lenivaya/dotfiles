{
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; {
  user.packages = with pkgs; [
    dbeaver
    pgcli
    pgadmin4-desktopmode
  ];

  services = {
    # pgadmin =
    #   enabled
    #   // {
    #     initialEmail = "user@gmail.com";
    #     openFirewall = true;
    #     initialPasswordFile = "${config.user.home}/.secrets/pgadmin_password";
    #   };

    postgresql =
      enabled
      // {
        package = pkgs.postgresql_16;

        # This crutch is here because some services cannot work via a UNIX
        # socket connection and I can't be bothered to configure proper
        # authentication.
        authentication = ''
          local all all trust
        '';
      };
  };

  user.extraGroups = ["postgres"];
}
