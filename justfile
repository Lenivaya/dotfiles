default:
  just --list

switch:
   nh os switch $DOTFILES -- --impure --show-trace

format:
  nix fmt

check:
  nix flake check

commit:
  git add . && commit-date --no-verify --no-edit

# build-iso:
#   nix build .#nixosConfigurations.iso.config.system.build.isoImage --impure --show-trace
