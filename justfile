default:
  just --list

build-iso:
  nix build .#nixosConfigurations.iso.config.system.build.isoImage --impure --show-trace
