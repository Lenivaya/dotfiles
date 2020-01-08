setopt auto_cd
setopt auto_name_dirs
setopt auto_pushd
setopt auto_resume
setopt cdable_vars
setopt clobber
setopt combining_chars
setopt hash_list_all
setopt ignore_eof
setopt long_list_jobs
setopt magic_equal_subst
setopt no_auto_name_dirs
setopt no_beep
setopt no_bg_nice
setopt no_check_jobs
setopt no_correct_all
setopt no_glob_dots
setopt no_hup
setopt no_mail_warning
setopt notify
setopt prompt_subst
setopt pushd_ignore_dups
setopt pushd_silent
setopt pushd_to_home
setopt rc_quotes

setopt append_history
setopt bang_hist
setopt extended_history
setopt hist_beep
setopt hist_expire_dups_first
setopt hist_ignore_all_dups
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_reduce_blanks
setopt hist_save_no_dups
setopt hist_verify
setopt inc_append_history
setopt share_history

autoload -Uz compinit
if [[ -n ${ZDOTDIR}/.zcompdump(#qN.mh+24) ]]; then
	compinit;
else
	compinit -C; fi; 

setopt auto_list
setopt auto_menu
setopt auto_param_keys
setopt auto_param_slash
setopt complete_in_word
setopt flow_control
setopt no_always_to_end
setopt no_case_glob
setopt no_complete_aliases
setopt no_menu_complete
setopt path_dirs


setopt magic_equal_subst

source '/home/leniviy/.zplugin/bin/zplugin.zsh'
autoload -Uz _zplugin
(( ${+_comps} )) && _comps[zplugin]=_zplugin

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
zplugin snippet OMZ::lib/completion.zsh


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
zplugin ice wait lucid
zplugin light spwhitt/nix-zsh-completions
zplugin ice wait lucid
zplugin light ninrod/pass-zsh-completion
# zplugin ice wait lucid
# zplugin light chisui/zsh-nix-shell
zplugin light mdarocha/zsh-windows-title

# fzf
zplugin ice wait lucid from"gh-r" as"program"
zplugin light junegunn/fzf-bin
[ -f ~/.fzf-keys.zsh ] && source ~/.fzf-keys.zsh

eval "$(starship init zsh)"
