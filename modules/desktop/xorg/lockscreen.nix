{ config, pkgs, ... }:

{
  services.xserver.xautolock = {
    enable = true;
    time = 5;
    locker = "${pkgs.betterlockscreen}/bin/betterlockscreen -l dim";
    notifier = ''
      ${pkgs.libnotify}/bin/notify-send "Locking in 10 seconds"
    '';
    extraOptions = [ "-lockaftersleep" "-corners ----" ];
  };

  my.packages = with pkgs.unstable; [ betterlockscreen ];

  my.home.xdg.configFile."betterlockscreenrc" = {
    source = <config/betterlockscreen/betterlockscreenrc>;
    recursive = true;
  };
}
