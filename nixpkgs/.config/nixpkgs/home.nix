{ config, pkgs, ... }:

{
  imports = [
    ./modules
  ];

  home.packages = with pkgs; [
    tmux
    maim scrot                # Screenshots
    sxhkd                     # X hotkey daemon
    neofetch
    htop
    usbutils unzip zip unrar
    ncdu                      # Disk space usage anlyzer
    ripgrep fd exa                # fast grepper; rust alternative to find; ls alternative
    android-file-transfer
    openvpn protonvpn-cli     # vpn
    sxiv
    zathura pandoc
    dunst
    xst
    nitrogen
    ps_mem
    pulsemixer
    pass
    xmobar

    # Appearance
    qt5ct lxappearance
    plasma5.breeze-qt5
    materia-theme
    paper-icon-theme
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
    jgmenu
    tdesktop
    discord
    spotify
    betterlockscreen
    qutebrowser chromium                            # Browsers
  ])
  ++
  (with pkgs.gnome3; [
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

  services = {
    kdeconnect.enable = true;

    redshift = {
      enable = true;
      provider = "geoclue2";
      extraOptions = [ "-m randr" ];
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

    kakoune = {
      enable = true;
      config.colorScheme = "gruvbox";
      config.numberLines = {
        enable = true;
        highlightCursor = true;
        relative = true;
        separator = "| ";
      };
      config.scrollOff = {
        columns = 4;
        lines = 4;
      };
      config.showMatching = true;
      config.showWhitespace = {
        enable = true;
        lineFeed = " ";
        space = " ";
        nonBreakingSpace = "⋅";
      };
      config.tabStop = 4;
      config.ui.enableMouse = true;
      config.wrapLines = {
        enable = true;
        indent = true;
        word = true;
        marker = "↪";
      };
    };

    rofi = {
      enable = true;
      lines = 10;
      font = "Iosevka 14";
      scrollbar = true;
      cycle = true;
      terminal = "${pkgs.unstable.alacritty}/bin/alacritty";
      theme = "solarized";
      extraConfig = ''
                      rofi.modi: window,run,ssh,drun
                    '';
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
