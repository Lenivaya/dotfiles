{ lib, ... }:
with lib;
{
  options.modules.themes = {
    colorScheme =
      let
        mkColor =
          default:
          mkOption {
            type = types.str;
            inherit default;
            description = "Color in a standard hexadecimal notation.";
            example = "#000000";
          };
      in
      {
        black = mkColor "#0b0806";
        red = mkColor "#844d2c";
        green = mkColor "#57553a";
        yellow = mkColor "#a17c38";
        blue = mkColor "#41434f";
        magenta = mkColor "#6b4444";
        cyan = mkColor "#59664c";
        white = mkColor "#a19782";

        brightBlack = mkColor "#2f2b2a";
        brightRed = mkColor "#a64848";
        brightGreen = mkColor "#897f5a";
        brightYellow = mkColor "#c8b38d";
        brightBlue = mkColor "#526274";
        brightMagenta = mkColor "#755c47";
        brightCyan = mkColor "#718062";
        brightWhite = mkColor "#c1ab83";

        background = mkColor "#0b0806";
        foreground = mkColor "#a19782";
      };
  };
}
