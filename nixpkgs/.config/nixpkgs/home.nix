{ pkgs, ... }:

{
  imports = [
    ./modules
  ];

  home.packages = with pkgs; [
    # some rust apps
    ripgrep               # fast grepper
    fd                    # rust alternative to find
    exa                   # ls alternative
    tokei                 # code statistic

    tmux
    maim scrot                          # Screenshots
    dunst                               # notification daemon
    neofetch                            # fetch
    htop
    usbutils unzip zip unrar
    ncdu                      # Disk space usage anlyzer
    android-file-transfer
    openvpn protonvpn-cli     # vpn
    sxiv
    zathura pandoc
    nitrogen
    ps_mem
    pulsemixer
    pass

    # Emacs as email client
    mu
    isync

    # Appearance
    qt5ct lxappearance
    plasma5.breeze-qt5
    materia-theme
    pywal wpgtk

    killall
    stow
  ]
  ++
  (with pkgs.unstable; [
    vim neovim vscode
    alacritty
    starship
    nnn
    ranger
    xmobar
    jgmenu
    discord
    spotify
    tdesktop
    betterlockscreen
    qutebrowser
    pfetch
  ])
  ++
  (with pkgs.gnome3; [
    adwaita-icon-theme
    nautilus file-roller gnome-autoar
    eog
  ])
  ++
  (with pkgs.pantheon; [
    elementary-camera
    elementary-calendar
    elementary-calculator
  ]);

  xdg.enable = true;

  gtk.enable = true;
  gtk = {
    iconTheme = {
      package = pkgs.paper-icon-theme;
      name = "Paper";
    };
    theme = {
      package = pkgs.gnome3.gnome_themes_standard;
      name = "Adwaita-dark";
    };
    gtk3.extraConfig = { gtk-decoration-layout = appmenu:none;
                         gtk-cursor-theme-name = "Adwaita"; gtk-cursor-theme-size=0; };
  };

  qt.enable = true;
  qt.platformTheme = "gnome";

  services = {
    kdeconnect.enable = true;

    redshift = {
      enable = true;
      provider = "geoclue2";
      extraOptions = [ "-m randr" ];
    };

    sxhkd = {
      enable = true;
      keybindings = { "super + Escape" = "pkill -USR1 -x sxhkd";
                      "super + KP_Left" = "st -e ranger";
                      "super + shift + KP_Left" = "st -e nnn";
                      "super + KP_Home" = "st -e tmux";
      };
    };

    compton = {
     enable = true;
     package = pkgs.unstable.picom;
     activeOpacity = "1.0";
     inactiveOpacity = "0.8";
     opacityRule = [ "100:name *= 'i3lock'"
                     "96:name *?= 'xmobar'"
                     "95:class_g *?= 'emacs'"
                     "75:class_g = 'Code'"
                     "75:class_g = 'code'"
                     "95:class_g *?= 'tabbed'"
                     "0:_NET_WM_STATE@:32a *= '_NET_WM_STATE_HIDDEN'"
                     "96:_NET_WM_STATE@:32a *= '_NET_WM_STATE_STICKY'" ];
     backend = "glx";
     blurExclude =  [ "window_type = 'dock'" "window_type = 'desktop'" ];
     fade = true;
     fadeDelta = 5;
     fadeSteps = [ "0.03" "0.03"];
     shadow = true;
     shadowOffsets = [ 1 1 ];
     shadowOpacity = "0.3";
     vSync = "opengl-swc";
    };

    gpg-agent = {
      enable = true;
      defaultCacheTtl = 1800;
      enableSshSupport = true;
    };
  };

  programs = {
    broot.enable = true;
    bat.enable = true;
    fzf.enable = true;
    # password-store = {
    #   enable = true;
    #   settings = {
    #     PASSWORD_STORE_DIR = "$XDG_DATA_HOME/password-store";
    #     PASSWORD_STORE_CLIP_TIME = "60";
    #   };
    # };

    rofi = {
      enable = true;
      lines = 10;
      font = "Iosevka 14";
      scrollbar = true;
      cycle = true;
      terminal = "${pkgs.unstable.alacritty}/bin/alacritty";
      theme = "gruvbox-dark-hard";
      extraConfig = ''
                      rofi.modi: drun
                    '';
    };

    chromium = {
      enable = true;
      package = pkgs.unstable.brave;
      extensions = [
                     "cjpalhdlnbpafiamejdnhcphjbkeiagm" # ublock origin
                     "hfjbmagddngcpeloejdejnfgbamkjaeg" # vimium c
                     #"cglpcedifkgalfdklahhcchnjepcckfn" # newtab adapter
                     "klbibkeccnjlkjkiokjodocebajanakg" # the great suspender
                     "hkgfoiooedgoejojocmhlaklaeopbecg" # picture-in-picture
                   ];
    };

    git = {
      userName = "Lenivaya";
      userEmail = "xocada@gmail.com";
    };

    home-manager = {
      enable = true;
      path = "…";
    };
  };
}
