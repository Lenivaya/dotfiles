[![NixOS 21.05](https://img.shields.io/badge/NixOS-v20.09-blue.svg?style=flat-square&logo=NixOS&logoColor=white)](https://nixos.org)
[![CI](https://github.com/Lenivaya/dotfiles/actions/workflows/CI.yml/badge.svg)](https://github.com/Lenivaya/dotfiles/actions/workflows/CI.yml)

<h1 align="center">dotfiles</h1>
<p align="center">Different dotfiles which i use every day</p><br>

![screenshot](https://user-images.githubusercontent.com/49302467/96636356-df89cd80-1325-11eb-9c91-b8861cfbbbaf.png)

<p align="center">
<span><img src="https://user-images.githubusercontent.com/49302467/96636467-ffb98c80-1325-11eb-855a-3bc7d97df150.png" height="187" /></span>
<span><img src="https://user-images.githubusercontent.com/49302467/96636725-5fb03300-1326-11eb-8040-d2f8a808d186.png" height="187" /></span>
<span><img src="https://user-images.githubusercontent.com/49302467/96636790-73f43000-1326-11eb-9283-2e9593b4e246.png" height="187" /></span>
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
