function __async_my_abbr_adder --on-event fish_prompt
    abbr nixossw 'nh os switch -- --impure --show-trace'

    abbr j just

    # Tmux
    abbr t tmux
    abbr tc 'tmux attach'
    abbr ta 'tmux attach -t'
    abbr tad 'tmux attach -d -t'
    abbr ts 'tmux new -s'
    abbr tl 'tmux ls'
    abbr tk 'tmux kill-session -t'
    abbr mux tmuxinator

    # Editor
    abbr vim nvim
    abbr vi nvim
    abbr v nvim
    abbr n nvim
    abbr edit nvim
    abbr nvimc 'nvim --clean'

    # Eza
    abbr eza 'eza --icons=auto --group-directories-first --header --hyperlink'
    abbr ls 'eza --icons=auto --group-directories-first --header --hyperlink'
    abbr l 'eza --icons=auto --group-directories-first --header --hyperlink -blF'
    abbr ll 'eza --icons=auto --group-directories-first --header --hyperlink -al'
    abbr llm 'eza --icons=auto --group-directories-first --header --hyperlink -al --sort=modified'
    abbr la 'eza --icons=auto --group-directories-first --header --hyperlink -abghilmu'
    abbr lx 'eza --icons=auto --group-directories-first --header --hyperlink -abghilmuHSU@'
    abbr tree 'eza --icons=auto --group-directories-first --header --hyperlink --tree'

    # files
    abbr mkdir "mkdir -p"

    # Memory
    abbr mem 'sudo ps_mem'
    abbr memf 'sudo ps_mem | fzf'

    # Rust
    abbr carup 'cargo update && cargo upgrade'

    # journalctl
    abbr jb "journalctl -b"
    abbr jf "journalctl --follow"
    abbr jbf "journalctl -b --follow"
    abbr jg "journalctl -b --grep"
    abbr ju "journalctl --unit"
    abbr juu "journalctl --user --unit"

    # Node
    abbr npmup "pnpm update -i -r --latest"
    abbr npm pnpm
    abbr yarn pnpm
    abbr npx "pnpm dlx"

    # Files
    abbr rmrf 'rm -rf'

    # Misc
    abbr twitch 'twitch-hls-client channel best -p mpv'
    abbr twitchau 'twitch-hls-client channel audio_only -p mpv'
    abbr xmr 'xmonad --recompile; xmonad --restart'
    abbr linkconfig 'ln -s $DOTFILES/config/sth ~/.config/sth'
    abbr nishell 'nix-shell -p '

    # git
    abbr gamend "git add . && git commit --amend --no-edit --no-verify"
    abbr gs "git status"
    abbr gpf "git push --force"
    abbr gp "git push"
    abbr gc "git commit -m"
    abbr gca "git add . && git commit -m"
    abbr gl "git log --oneline --graph --all --decorate"

    # Notes
    abbr oo "nvim ~/Sync/Knowledge-base/"
    abbr os "fd '.md\$' ~/Knowledge-base/ | shuf -n 1 | xargs nvim" # opens random note
end
