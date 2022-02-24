HISTSIZE=100000   # Max events to store in internal history.
SAVEHIST=100000   # Max events to store in history file.

if [[ ! -d $HOME/.zinit ]]; then
    print -P "Installing Plugin Manager (zdharma/zinit)…"
    command mkdir -p ~/.zinit && mkdir -p ~/.zinit/completions
    command git clone https://github.com/zdharma-continuum/zinit ~/.zinit/bin && \
        print -P "%F{33}▓▒░ %F{34}Installation successful.%f" || \
        print -P "%F{160}▓▒░ The clone has failed.%f"
fi


_set_cursor() {
   echo -ne '\e[5 q'
}
precmd_functions+=(_set_cursor)

## System provided completions
fpath+=( /run/current-system/sw/share/zsh/site-functions )

source ~/.zinit/bin/zinit.zsh
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Plugins
## Snippets

zinit wait lucid light-mode for \
        OMZP::sudo/sudo.plugin.zsh \
        OMZP::git-auto-fetch/git-auto-fetch.plugin.zsh \
        OMZL::grep.zsh \
        OMZL::compfix.zsh \
        OMZL::directories.zsh \
        OMZL::spectrum.zsh \
        OMZL::theme-and-appearance.zsh

zinit light-mode for \
        OMZL::key-bindings.zsh \
        OMZL::termsupport.zsh \
        OMZL::history.zsh \

zinit wait"0a" lucid for \
        zpm-zsh/colors \
        softmoth/zsh-vim-mode \

zinit wait"0b" lucid light-mode for \
    pick'autopair.zsh' nocompletions \
        hlissner/zsh-autopair \
    atinit"ZINIT[COMPINIT_OPTS]=-C; zicompinit; zicdreplay" \
        zdharma-continuum/fast-syntax-highlighting \
    silent atload"!_zsh_autosuggest_start && bindkey '^ ' autosuggest-accept" \
        zsh-users/zsh-autosuggestions \
    atload"zicompinit; zicdreplay" blockf \
        zsh-users/zsh-completions \

## In turbo mode
zinit wait"0c" lucid light-mode for \
        Aloxaf/fzf-tab \
        pick'shell/*.plugin.zsh' \
                denisidoro/navi \
        joshskidmore/zsh-fzf-history-search \
        zsh-users/zsh-history-substring-search \
        Tarrasch/zsh-bd \
        mafredri/zsh-async \
        ael-code/zsh-colored-man-pages \
        MichaelAquilina/zsh-auto-notify \
        wfxr/forgit \
        soimort/translate-shell \
        zdharma-continuum/zui \
        zdharma-continuum/zinit-console \
        sindresorhus/pretty-time-zsh \
        laggardkernel/zsh-thefuck \
        hermitmaster/zsh-exa-plugin  \
        changyuheng/fz \
        rupa/z \
        chisui/zsh-nix-shell \
        kutsan/zsh-system-clipboard \
        b4b4r07/emoji-cli \
        sinetoami/web-search \
        reegnz/jq-zsh-plugin \
        micrenda/zsh-nohup \
        rtuin/zsh-case \
        katrinleinweber/oh-my-zsh-youtube-dl-aliases \
    as"program" \
        pick"tldr" raylee/tldr \
    as"completion" \
        OMZP::docker/_docker \
        OMZL::completion.zsh \
        ninrod/pass-zsh-completion \
        spwhitt/nix-zsh-completions \
        lukechilds/zsh-better-npm-completion \
        srijanshetty/zsh-pandoc-completion \
        thetic/extract \
        atinit='ZSH_BASH_COMPLETIONS_FALLBACK_LAZYLOAD_DISABLE=true' \
            3v1n0/zsh-bash-completions-fallback

source $XDG_CONFIG_HOME/zsh/br

eval "$(starship init zsh)"

## Auto-generated by my nix config
source $ZDOTDIR/extra.zshrc
