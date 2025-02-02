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
    abbr edit nvim
    abbr nvimpure 'nvim --clean'

    # Eza
    abbr eza 'eza --icons=auto --group-directories-first --header --hyperlink'
    abbr ls 'eza --icons=auto --group-directories-first --header --hyperlink'
    abbr l 'eza --icons=auto --group-directories-first --header --hyperlink -blF'
    abbr ll 'eza --icons=auto --group-directories-first --header --hyperlink -al'
    abbr llm 'eza --icons=auto --group-directories-first --header --hyperlink -al --sort=modified'
    abbr la 'eza --icons=auto --group-directories-first --header --hyperlink -abghilmu'
    abbr lx 'eza --icons=auto --group-directories-first --header --hyperlink -abghilmuHSU@'
    abbr tree 'eza --icons=auto --group-directories-first --header --hyperlink --tree'

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
    abbr npm pnpm
    abbr yarn pnpm

    # Misc
    abbr twitch twitch-hls-client channel best -p mpv
end
