{
  config,
  lib,
  home-manager,
  ...
}:
with lib;
with lib.my; {
  config = mkMerge [
    (mkIf config.this.isHeadful {
      home-manager.users.${config.user.name}.manual = {
        html.enable = false;
        json.enable = false;
        manpages.enable = true;
      };

      documentation = {
        enable = true;
        doc.enable = false;
        info.enable = false;
      };
    })
    (mkIf config.this.isHeadless {
      home-manager.users.${config.user.name}.manual.manpages.enable = false;
      documentation.enable = false;
    })
  ];
}
