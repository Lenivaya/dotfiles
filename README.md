[![NixOS 20.09](https://img.shields.io/badge/NixOS-v20.09-blue.svg?style=flat-square&logo=NixOS&logoColor=white)](https://nixos.org)

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
| **font:**    | Iosevka / IBM Plex                                                    |
| **browser:** | google-chrome / Firefox                                               |
| **editor:**  | Emacs                                                                 |
| **term:**    | st                                                                    |

---

## Management

Behold. `bin/hey`.

| Command                    | Description                                                                |
| -------------------------- | -------------------------------------------------------------------------- |
| `hey check`                | Run tests and checks for this flake                                        |
| `hey gc`                   | Runs `nix-collect-garbage -d`. Use `--all` to clean up system profile too. |
| `hey rebuild`              | Rebuild this flake (shortcut: `hey re`)                                    |
| `hey rollback`             | Roll back to previous system generation                                    |
| `hey show`                 | Show flake outputs of this repo                                            |
| `hey ssh REMOTE [COMMAND]` | Run a `bin/hey` command on REMOTE over ssh                                 |
| `hey upgrade`              | Update flake lockfile and switch to it (shortcut: `hey up`)                |
