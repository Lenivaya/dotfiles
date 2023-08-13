{
  config,
  options,
  lib,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.bootsplash;
in {
  options.modules.bootsplash.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    boot.plymouth = enabled;

    # allow plymouth to take over the framebuffer sooner
    console.earlySetup = true;

    # make the boot quiet
    boot.consoleLogLevel = 3;
    boot.initrd.verbose = false;

    boot.kernelParams = [
      # prevent the kernel from blanking plymouth out of the fb
      "fbcon=nodefer"
      # disable boot logo if any
      "logo.nologo"
      # tell the kernel to not be verbose
      "quiet"
      # disable systemd status messages
      "rd.systemd.show_status=auto"
      # lower the udev log level to show only errors or worse
      "rd.udev.log_level=3"
      # disable the cursor in vt to get a black screen during intermissions
      # TODO turn back on in tty
      "vt.global_cursor_default=0"
    ];

    # powerManagement = with pkgs; {
    #   powerDownCommands = ''
    #     ${getExe plymouth} --show-splash
    #   '';
    #   resumeCommands = ''
    #     ${getExe plymouth} --quit
    #   '';
    # };
  };
}
