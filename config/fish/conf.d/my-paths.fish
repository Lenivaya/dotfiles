function __async_my_paths_adder --on-event fish_prompt
    set -x fish_user_paths

    # Embrace impurity.
    fish_add_path ~/.config/dotfiles/bin/
    fish_add_path /bin
    fish_add_path ~/.cargo/bin
    fish_add_path ~/.local/bin
    fish_add_path ~/.luarocks/bin
    fish_add_path ~/Library/Python/3.{8,9}/bin
    fish_add_path /usr/local/opt/sqlite/bin
    fish_add_path /usr/local/sbin
    fish_add_path ~/.gem/ruby/2.6.0/bin
    fish_add_path ~/.local/bin/pnpm-bins
    fish_add_path ~/.cache/npm/
    fish_add_path ~/.local/share/bob-nvim/bin
    fish_add_path ~/.local/share/bob-nvim/nvim-linux64/bin
    fish_add_path /var/lib/flatpak/exports/bin/
    fish_add_path ~/.local/share/mise/shims

    # pnpm
    set -gx PNPM_HOME "/home/leniviy/.local/share/pnpm"
    if not string match -q -- $PNPM_HOME $PATH
        set -gx PATH "$PNPM_HOME" $PATH
    end
    # pnpm end

    # golang
    if command --query go
        contains -- (go env GOROOT)/bin $fish_user_paths
        or fish_add_path --prepend --move --path (go env GOROOT)/bin

        contains -- (go env GOPATH)/bin $fish_user_paths
        or fish_add_path --prepend --move --path (go env GOPATH)/bin
    end

    # .NET configurations
    if command -q dotnet
        fish_add_path $HOME/.dotnet/tools
        fish_add_path ~/.dotnet/tools
    end
end
