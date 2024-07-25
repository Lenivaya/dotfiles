[![NixOS 23.05](https://img.shields.io/badge/NixOS-v23.05-blue.svg?style=flat-square&logo=NixOS&logoColor=white)](https://nixos.org)
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

|              |                                                                                 |
| ------------ | ------------------------------------------------------------------------------- |
| **OS:**      | NixOS                                                                           |
| **WM:**      | XMonad                                                                          |
| **sh:**      | zsh (with [fastest plugin-manager](https://github.com/zdharma-continuum/zinit)) |
| **font:**    | (Iosevka + IBM Plex) or Pragmata-Pro                                            |
| **browser:** | google-chrome / Firefox                                                         |
| **editor:**  | Emacs                                                                           |
| **term:**    | st                                                                              |

---

## Quick start

1. Acquire NixOS 23.05 or newer:

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
   # Set USER to your desired username (defaults to leniviy)
   USER=...

   git clone https://github.com/lenivaya/dotfiles /etc/dotfiles
   cd /etc/dotfiles

   # Create a host config in `hosts/` and add it to the repo:
   mkdir -p hosts/$HOST
   nixos-generate-config --root /mnt --dir /etc/dotfiles/hosts/$HOST
   rm -f hosts/$HOST/configuration.nix
   cp hosts/t440p/default.nix hosts/$HOST/default.nix
   vim hosts/$HOST/default.nix  # configure this for your system; don't use it verbatim!
   git add hosts/$HOST

   # Install nixOS
   USER=$USER nixos-install --root /mnt --impure --flake .#$HOST

   # If you get 'unrecognized option: --impure', replace '--impure' with
   # `--option pure-eval no`.


   # Then move the dotfiles to the mounted drive!
   mv /etc/dotfiles /mnt/etc/dotfiles
   ```

> :warning: **Don't forget to change your `root` and `$USER` passwords!** They
> are set to `nixos` by default.
