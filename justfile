default:
  just --list

switch:
   nh os switch -- --impure --show-trace

# build-iso:
#   nix build .#nixosConfigurations.iso.config.system.build.isoImage --impure --show-trace
