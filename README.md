[![NixOS 21.05](https://img.shields.io/badge/NixOS-v21.05-blue.svg?style=flat-square&logo=NixOS&logoColor=white)](https://nixos.org)
[![Tests](https://github.com/Lenivaya/dotfiles/actions/workflows/tests.yml/badge.svg)](https://github.com/Lenivaya/dotfiles/actions/workflows/tests.yml)

<h1 align="center">dotfiles</h1>
<p align="center">Different dotfiles which i use every day</p><br>

![screenshot](https://user-images.githubusercontent.com/49302467/128747027-cf246e08-4159-4b9e-9149-3575962ccf9d.png)

<p align="center">
<span><img src="https://user-images.githubusercontent.com/49302467/128747085-ad7cd291-059c-4cec-8950-68765f6bcd9e.png" height="187" /></span>
<span><img src="https://user-images.githubusercontent.com/49302467/128747161-bac98f10-e431-4134-8f12-aeedfad53067.png" height="187" /></span>
<span><img src="https://user-images.githubusercontent.com/49302467/128747069-ff71aeca-f216-438e-a42f-41e91a90c9dc.png" height="187" /></span>
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
