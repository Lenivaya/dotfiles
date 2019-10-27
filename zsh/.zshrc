#!/usr/bin/env zsh

setopt autocd
setopt extended_glob
setopt no_case_glob
setopt prompt_subst

zstyle ':completion:*' format '%d'
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-dirs-first true
zstyle ':completion:*' menu select
zstyle ':completion:*' separate-sectoins true
zstyle ':completion:*:cd:*' ignore-parents parent pwd
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true
zstyle ':completion:*:functions' ignored-patterns '_*'

autoload -Uz compinit
if [[ -n ${ZDOTDIR}/.zcompdump(#qN.mh+24) ]]; then
	compinit;
else
	compinit -C; fi; 

### Added by Zplugin's installer
source '/home/leniviy/.zplugin/bin/zplugin.zsh'
autoload -Uz _zplugin
(( ${+_comps} )) && _comps[zplugin]=_zplugin
### End of Zplugin's installer chunk

# Plugins
zplugin ice wait lucid
zplugin snippet OMZ::plugins/sudo/sudo.plugin.zsh
zplugin ice wait lucid
zplugin snippet OMZ::lib/theme-and-appearance.zsh
zplugin ice wait lucid
zplugin snippet OMZ::lib/grep.zsh
zplugin snippet OMZ::lib/key-bindings.zsh
zplugin snippet OMZ::lib/history.zsh
zplugin snippet OMZ::plugins/vi-mode/vi-mode.plugin.zsh


zplugin ice wait"0b" lucid pick'autopair.zsh' nocompletions
zplugin light hlissner/zsh-autopair
zplugin ice wait"0c" lucid atload"_zsh_autosuggest_start"
zplugin light zsh-users/zsh-autosuggestions
zplugin ice wait"1" lucid atinit"ZPLGM[COMPINIT_OPTS]=-C; zpcompinit; zpcdreplay"
zplugin light zdharma/fast-syntax-highlighting
zplugin ice blockf; zplugin light zsh-users/zsh-completions

zplugin light zsh-users/zsh-history-substring-search
zplugin light Tarrasch/zsh-bd
zplugin light mafredri/zsh-async
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
