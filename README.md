[![NixOS 21.05](https://img.shields.io/badge/NixOS-v21.05-blue.svg?style=flat-square&logo=NixOS&logoColor=white)](https://nixos.org)
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

1. Download the latest build of [NixOS 21.05][nixos].
2. Boot into the installer.
3. Do your partitions and mount your root to `/mnt` 
4. Install these dotfiles:
5. `nix-shell -p git nixFlakes`
6. `git clone https://github.com/lenivaya/dotfiles /mnt/etc/nixos`
7. Install NixOS: `nixos-install --root /mnt --flake /mnt/etc/nixos#XYZ`, where
   `XYZ` is [the host you want to install](hosts/).  Use `#generic` for a
   simple, universal config, or create a sub-directory in `hosts/` for your device.
8. Reboot!
9. Change your `root` and `$USER` passwords!

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
