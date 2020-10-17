[![NixOS 20.09](https://img.shields.io/badge/NixOS-v20.09-blue.svg?style=flat-square&logo=NixOS&logoColor=white)](https://nixos.org)

<h1 align="center">dotfiles</h1>
<p align="center">Different dotfiles which i use every day</p><br>

![screenshot](https://user-images.githubusercontent.com/49302467/82123117-4d8eff00-97a0-11ea-96ea-a8e5d7bfa345.png)

<p align="center">
<span><img src="https://user-images.githubusercontent.com/49302467/81593317-eea83f00-93c7-11ea-9cf1-8e237113cdfd.png" height="187" /></span>
<span><img src="https://user-images.githubusercontent.com/49302467/81593354-fbc52e00-93c7-11ea-9020-6b66b0717659.png" height="187" /></span>
<span><img src="https://user-images.githubusercontent.com/49302467/81593008-7d688c00-93c7-11ea-8229-7f7c9144cde2.png" height="187" /></span>
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

| Command           | Description                                                     |
|-------------------|-----------------------------------------------------------------|
| `hey rebuild`     | Rebuild this flake (shortcut: `hey re`)                         |
| `hey upgrade`     | Update flake lockfile and switch to it (shortcut: `hey up`)     |
| `hey rollback`    | Roll back to previous system generation                         |
| `hey gc`          | Runs `nix-collect-garbage -d`. Use sudo to clean system profile |
| `hey push REMOTE` | Deploy these dotfiles to REMOTE (over ssh)                      |
| `hey check`       | Run tests and checks for this flake                             |
| `hey show`        | Show flake outputs of this repo                                 |
