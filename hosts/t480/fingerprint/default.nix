{ inputs, lib, ... }:
with lib;
with my;
{
  # imports = with inputs; [
  #   #   nixos-06cb-009a-fingerprint-sensor.nixosModules.open-fprintd
  #   #   nixos-06cb-009a-fingerprint-sensor.nixosModules.python-validity
  #   # nixos-06cb-009a-fingerprint-sensor.nixosModules."06cb-009a-fingerprint-sensor"
  # ];

  # services.fprintd.enable = mkForce false;
  # services.open-fprintd.enable = mkForce true;
  # services.python-validity.enable = mkForce true;
  #
  # services."06cb-009a-fingerprint-sensor" = {
  #   enable = true;
  #   backend = "python-validity";
  # };
  #
  # services."06cb-009a-fingerprint-sensor" = {
  #   enable = true;
  #   backend = "libfprint-tod";
  #   calib-data-file = ./calib-data.bin;
  # };

  # services.fprintd = {
  #   enable = true;
  #   tod = {
  #     enable = true;
  #     driver = inputs.nixos-06cb-009a-fingerprint-sensor.lib.libfprint-2-tod1-vfs0090-bingch {
  #       calib-data-file = ./calib-data.bin;
  #     };
  #   };
  # };
}
