export ZSH="/home/leniviy/.oh-my-zsh"

source $ZSH/oh-my-zsh.sh

autoload -Uz compinit
if [[ -n ${ZDOTDIR}/.zcompdump(#qN.mh+24) ]]; then
	compinit;
else
	compinit -C;
fi;

### Added by Zplugin's installer
source '/home/leniviy/.zplugin/bin/zplugin.zsh'
autoload -Uz _zplugin
(( ${+_comps} )) && _comps[zplugin]=_zplugin
### End of Zplugin's installer chunk

# Plugins
zplugin ice wait"0a" lucid
zplugin snippet OMZ::plugins/sudo/sudo.plugin.zsh

zplugin ice wait"0b" lucid pick'autopair.zsh' nocompletions
zplugin light hlissner/zsh-autopair

zplugin ice wait"0c" lucid atload"_zsh_autosuggest_start"
zplugin light zsh-users/zsh-autosuggestions

zplugin ice wait"1" lucid atinit"ZPLGM[COMPINIT_OPTS]=-C; zpcompinit; zpcdreplay"
zplugin light zdharma/fast-syntax-highlighting

zplugin snippet OMZ::plugins/vi-mode/vi-mode.plugin.zsh
zplugin light zsh-users/zsh-history-substring-search
zplugin light Tarrasch/zsh-bd

zplugin light mafredri/zsh-async
zplugin ice blockf; zplugin light zsh-users/zsh-completions
zplugin light ael-code/zsh-colored-man-pages
zplugin light MichaelAquilina/zsh-autoswitch-virtualenv
zplugin light wfxr/forgit

# fzf
zplugin ice wait lucid from"gh-r" as"program"
zplugin light junegunn/fzf-bin
zplugin light hschne/fzf-git
[ -f ~/.fzf.colors ] && source ~/.fzf.colors
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

eval "$(starship init zsh)"
