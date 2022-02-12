{ config, lib, pkgs, ... }:

{
  services.clight = {
    enable = lib.mkDefault true;

    settings = {
      verbose = true;

      backlight = {
        ac_timeouts = [ 120 300 60 ];
        pause_on_lid_closed = true;
        capture_on_lid_opened = true;
      };

      # sensor = {
      #   ac_regression_points =
      #     [ 0.0 0.12 0.18 0.28 0.42 0.49 0.54 0.65 0.75 0.78 0.8 ];
      #   batt_regression_points =
      #     [ 0.0 0.12 0.18 0.28 0.42 0.49 0.54 0.65 0.75 0.78 0.8 ];
      #   captures = [ 16 16 ];
      # };

      gamma.long_transition = true;

      #   screen = {
      #     contrib = 0.2;
      #     num_samples = 20;
      #   };
    };
  };

  systemd.user.services.clight.path = with pkgs; [ gawk ];
}
