dfault:
  just --list

switch:
   nh os switch $DOTFILES -- --impure --show-trace --accept-flake-config

format:
  nix fmt

check:
  nix flake check

commit:
  git add . && commit-date --no-verify --no-edit

bump-firefox:
  nix flake update firefox firefox-csshacks betterfox stevenblack-hosts

bump-xmonad:
  nix flake update xmonad xmonad-contrib xmonad-extras

# build-iso:
#   nix build .#nixosConfigurations.iso.config.system.build.isoImage --impure --show-trace
