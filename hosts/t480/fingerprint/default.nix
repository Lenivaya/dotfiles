{ inputs, lib, ... }:
with lib;
with my;
{
  # imports = with inputs; [
  #   nixos-06cb-009a-fingerprint-sensor.nixosModules.open-fprintd
  #   nixos-06cb-009a-fingerprint-sensor.nixosModules.python-validity
  # ];

  # services.fprintd.enable = mkForce false;
  # services.open-fprintd.enable = mkForce true;
  # services.python-validity.enable = mkForce true;

  services.fprintd = {
    enable = true;
    tod = {
      enable = true;
      driver = inputs.nixos-06cb-009a-fingerprint-sensor.lib.libfprint-2-tod1-vfs0090-bingch {
        calib-data-file = ./calib-data.bin;
      };
    };
  };
}
