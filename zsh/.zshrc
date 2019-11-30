#!/usr/bin/env zsh

function set_term_title(){
  echo -ne "\033]0;${USER}@${HOST} ${PWD}\007";
}
precmd_functions+=(set_term_title)
setopt auto_cd
setopt auto_name_dirs
setopt auto_pushd
setopt cdable_vars
setopt no_case_glob
setopt no_case_glob
setopt prompt_subst
setopt pushd_silent
setopt pushd_to_home

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

setopt magic_equal_subst

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
zplugin ice wait lucid
zplugin snippet OMZ::lib/compfix.zsh
zplugin ice wait lucid
zplugin snippet OMZ::lib/directories.zsh
zplugin ice wait lucid
zplugin snippet OMZ::lib/spectrum.zsh
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

zplugin ice wait lucid
zplugin light zsh-users/zsh-history-substring-search
zplugin ice wait lucid
zplugin light Tarrasch/zsh-bd
zplugin ice wait"2" lucid
zplugin light mafredri/zsh-async
zplugin ice wait lucid
zplugin light ael-code/zsh-colored-man-pages
zplugin ice wait lucid
zplugin light MichaelAquilina/zsh-auto-notify
zplugin ice wait lucid
zplugin light wfxr/forgit
zplugin ice wait lucid
zplugin light denisidoro/navi

# fzf
zplugin ice wait lucid from"gh-r" as"program"
zplugin light junegunn/fzf-bin
[ -f ~/.fzf.colors ] && source ~/.fzf.colors
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

eval "$(starship init zsh)"
