{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.term.st;
in {
  options.modules.desktop.term.st.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      (st.overrideAttrs (_oldAttrs: {
        buildInputs = with pkgs.xorg;
          [libX11 libXft libXcursor] ++ (with pkgs; [harfbuzz]);
        src = fetchgit {
          url = "https://github.com/Lenivaya/st";
          sha256 = "N9q/s3y5243zLzkY9JRrFQF3PIrOEaGvV2Nl5XF1ksg=";
        };
      }))

      (makeDesktopItem {
        name = "st";
        desktopName = "Suckless Terminal";
        genericName = "Default terminal";
        icon = "utilities-terminal";
        exec = "st";
        categories = ["Development" "System" "Utility"];
      })
    ];
  };
}
