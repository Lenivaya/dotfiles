final: prev: {
  rofi-screenshot = prev.rofi-screenshot.overrideAttrs (oa: {
    postFixup = ''
      wrapProgram $out/bin/${oa.pname} \
        --set PATH ${
          with final;
          lib.makeBinPath [
            libnotify
            slop
            ffcast
            ffmpeg
            xclip
            rofi
            coreutils
            gnused
            procps
            gawk # fixes screen recording feature
          ]
        }
    '';
  });
}
