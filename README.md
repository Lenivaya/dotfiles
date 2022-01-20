[![NixOS 21.11](https://img.shields.io/badge/NixOS-v21.11-blue.svg?style=flat-square&logo=NixOS&logoColor=white)](https://nixos.org)
[![Tests](https://github.com/Lenivaya/dotfiles/actions/workflows/tests.yml/badge.svg)](https://github.com/Lenivaya/dotfiles/actions/workflows/tests.yml)

<h1 align="center">dotfiles</h1>
<p align="center">Different dotfiles which i use every day</p><br>

![screenshot](https://user-images.githubusercontent.com/49302467/131035574-fdc95dda-e94e-450e-9196-b550d5c18ffa.png)

<p align="center">
<span><img src="https://user-images.githubusercontent.com/49302467/131035586-8a527147-30a2-488c-af72-d7dde2177a63.png" height="187" /></span>
<span><img src="https://user-images.githubusercontent.com/49302467/131035991-ea023d5a-d2f8-43d5-8a03-8c1d8728fd33.png" height="187" /></span>
<span><img src="https://user-images.githubusercontent.com/49302467/131035742-e0c7a574-1f0c-4121-95e6-e672ceda308e.png" height="187" /></span>
</p>

---

|              |                                                                       |
| ------------ | --------------------------------------------------------------------- |
| **OS:**      | NixOS                                                                 |
| **WM:**      | XMonad                                                                |
| **sh:**      | zsh (with [fastest plugin-manager](https://github.com/zdharma/zinit)) |
| **font:**    | (Iosevka + IBM Plex) or Pragmata-Pro                                  |
| **browser:** | google-chrome / Firefox                                               |
| **editor:**  | Emacs                                                                 |
| **term:**    | st                                                                    |

---

## Quick start

1. Acquire NixOS 21.11 or newer:

   ```sh
   # Download nixos-unstable
   wget -O nixos.iso https://channels.nixos.org/nixos-unstable/latest-nixos-minimal-x86_64-linux.iso

   # Write it to a flash drive
   cp nixos.iso /dev/sdX
   ```

2. Boot into the installer.

3. Switch to root user: `sudo su -`

4. Do your partitions and mount your root to `/mnt` ([for
   example](https://nixos.org/manual/nixos/stable/index.html#sec-installation-partitioning)).

5. Install these dotfiles:

   ```sh
   nix-shell -p git nixFlakes

   # Set HOST to the desired hostname of this system
   HOST=...
   # Set USER to your desired username (defaults to hlissner)
   USER=...

   git clone https://github.com/lenivaya/dotfiles /mnt/etc/nixos
   cd /mnt/etc/nixos

   # Create a host config in `hosts/` and add it to the repo:
   mkdir -p hosts/$HOST
   nixos-generate-config --root /mnt --dir /etc/nixos/hosts/$HOST
   rm -f hosts/$HOST/configuration.nix
   cp hosts/t440p/default.nix hosts/$HOST/default.nix
   vim hosts/$HOST/default.nix  # configure this for your system; don't use it verbatim!
   git add hosts/$HOST

   # Install nixOS
   USER=$USER nixos-install --root /mnt --impure --flake .#$HOST

   # If you get 'unrecognized option: --impure', replace '--impure' with
   # `--option pure-eval no`.
   ```

> :warning: **Don't forget to change your `root` and `$USER` passwords!** They
> are set to `nixos` by default.

## Management

Behold. `bin/hey`.

```
Usage: hey [global-options] [command] [sub-options]

Available Commands:
  check                  Run 'nix flake check' on your dotfiles
  gc                     Garbage collect & optimize nix store
  generations            Explore, manage, diff across generations
  help [SUBCOMMAND]      Show usage information for this script or a subcommand
  rebuild                Rebuild the current system's flake
  repl                   Open a nix-repl with nixpkgs and dotfiles preloaded
  rollback               Roll back to last generation
  search                 Search nixpkgs for a package
  show                   [ARGS...]
  ssh HOST [COMMAND]     Run a bin/hey command on a remote NixOS system
  swap PATH [PATH...]    Recursively swap nix-store symlinks with copies (or back).
  test                   Quickly rebuild, for quick iteration
  theme THEME_NAME       Quickly swap to another theme module
  update [INPUT...]      Update specific flakes or all of them
  upgrade                Update all flakes and rebuild system

Options:
    -d, --dryrun                     Don't change anything; preform dry run
    -D, --debug                      Show trace on nix errors
    -f, --flake URI                  Change target flake to URI
    -h, --help                       Display this help, or help for a specific command
    -i, -A, -q, -e, -p               Forward to nix-env
```
