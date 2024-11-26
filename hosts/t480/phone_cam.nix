# Virtual camera and mic
{ config, ... }:
{
  boot.extraModulePackages = with config.boot.kernelPackages; [ v4l2loopback.out ];
  boot.kernelModules = [
    "v4l2loopback"
    "snd-aloop"
  ];
  boot.extraModprobeConfig = ''options v4l2loopback exclusive_caps=1 card_label="Virtual Camera"'';
  security.polkit.enable = true;
}
# ffmpeg -hide_banner -vcodec mjpeg -framerate 60 -s 1280x720 -i /dev/video1 -vcodec rawvideo -vf "format=yuv420p,boxblur=1" -f v4l2 /dev/video0
# ffmpeg -hide_banner -vcodec mjpeg -framerate 60 -s 1280x720 -i /dev/video1 -vcodec rawvideo -vf "format=yuv420p,boxblur=1,hflip" -f v4l2 /dev/video0
