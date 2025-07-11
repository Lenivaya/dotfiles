default:
  just --list

switch:
   nh os switch $DOTFILES -- --impure --show-trace --accept-flake-config \
   --option 'extra-substituters' 'https://chaotic-nyx.cachix.org/' --option extra-trusted-public-keys "chaotic-nyx.cachix.org-1:HfnXSw4pj95iI/n17rIDy40agHj12WfF+Gqk6SonIT8="

format:
  nix fmt

check:
  nix flake check

commit:
  git add . && commit-date --no-verify --no-edit

bump-firefox:
  nix flake update firefox firefox-csshacks betterfox stevenblack-hosts zen-browser

bump-browsers: bump-firefox
  nix flake update browser-previews

bump-xmonad:
  nix flake update xmonad xmonad-contrib xmonad-extras

bump-things:
  nix flake update spicetify-nix auto-cpufreq skippy-xd zcfan keyd stevenblack-hosts intellimacs nur picom resterrs home-manager programsdb nix-index-db chaotic

bump-hardware:
  nix flake update nixos-hardware nixos-facter-modules nixos-06cb-009a-fingerprint-sensor srvos

bump-unstable:
  nix flake update nixpkgs-unstable nixpkgs-unstable-small

bump-nixpkgs: bump-unstable
  nix flake update nixpkgs

gc:
  nh clean all && sudo nix-collect-garbage -d

add-proprietary:
  $DOTFILES/hosts/$HOSTNAME/add-proprietary.sh

# build-iso:
#   nix build .#nixosConfigurations.iso.config.system.build.isoImage --impure --show-trace
