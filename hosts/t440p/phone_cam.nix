# Virtual camera and mic
{
  config,
  lib,
  pkgs,
  ...
}: {
  boot.extraModulePackages = with config.boot.kernelPackages; [v4l2loopback.out];
  boot.kernelModules = ["v4l2loopback" "snd-aloop"];
  boot.extraModprobeConfig = ''options v4l2loopback exclusive_caps=1 card_label="Virtual Camera"'';
}
# ffmpeg -hide_banner -vcodec mjpeg -framerate 60 -s 640x480 -i /dev/video1 -vcodec rawvideo -vf format=yuv420p,"boxblur=1" -f v4l2 /dev/video0
# ffmpeg -hide_banner -vcodec mjpeg -framerate 60 -s 2560x1440 -i /dev/video1 -vcodec rawvideo -vf format=yuv420p,"boxblur=1" -f v4l2 /dev/video0
