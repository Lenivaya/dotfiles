[![NixOS 20.03](https://img.shields.io/badge/NixOS-v20.03-blue.svg?style=flat-square&logo=NixOS&logoColor=white)](https://nixos.org)

<h1 align="center">dotfiles</h1>
<p align="center">Different dotfiles which i use every day</p><br>

![screenshot](https://user-images.githubusercontent.com/49302467/82123117-4d8eff00-97a0-11ea-96ea-a8e5d7bfa345.png)

<p align="center">
<span><img src="https://user-images.githubusercontent.com/49302467/81593317-eea83f00-93c7-11ea-9cf1-8e237113cdfd.png" height="188" /></span>
<span><img src="https://user-images.githubusercontent.com/49302467/81593354-fbc52e00-93c7-11ea-9020-6b66b0717659.png" height="188" /></span>
<span><img src="https://user-images.githubusercontent.com/49302467/81593008-7d688c00-93c7-11ea-8229-7f7c9144cde2.png" height="188" /></span>
</p>

- OS: NixOS
- wm: xmonad
- sh: zsh (with [fastest plugin-manager](https://github.com/zdharma/zinit))
- font: Iosevka Term ss08
    
  You can build custom Iosevka in NixOs with something like this
  ```nix
  fonts = {
    fonts = with pkgs; [
      (iosevka.override {
        privateBuildPlan = {
          family = "Iosevka Term";
          design = [ "term" "ss08" ];
        };
        set = "term-ss08";
        }) 
    ];
  };
  ```
- browser: google-chrome / Firefox
- editor: Emacs
- term: st
