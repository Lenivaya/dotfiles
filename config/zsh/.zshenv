export PATH="$HOME/.npm-global/bin:$PATH"
export PATH="$HOME/.cabal/bin:$PATH"
export PATH="$HOME/.emacs.d/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/go/bin:$PATH"
export GOPATH=$HOME/go
export GOBIN=$GOPATH/bin
export LANG=en_US.UTF-8

# export QT_QPA_PLATFORMTHEME="qt5ct"
export PF_INFO="ascii title os kernel uptime pkgs shell wm"

export FZF_DEFAULT_OPTS="--height=50% --layout=reverse --info=inline --border --margin=1 --padding=1"

# Home-manager sessionVariables
source /etc/profiles/per-user/leniviy/etc/profile.d/hm-session-vars.sh

## Auto-generated by my nix config
[ -f "${0:a:h}/extra.zshenv" ] && source "${0:a:h}/extra.zshenv"

# If you have host-local configuration, this is where you'd put it
[ -f ~/.zshenv ] && source ~/.zshenv
