# unicode
set fish_function_path $fish_function_path ~/plugin-foreign-env/functions

if status is-interactive
    # Commands to run in interactive sessions can go here
    function __async_init --on-event fish_prompt
        # Only run once
        functions -e __async_init

        # Initialize tools in background
        zoxide init fish | source
        atuin init fish | source
        direnv hook fish | source
        nix-your-shell fish | source
    end
end

if test -e ~/.fish_extra_env.fish
    source ~/.fish_extra_env.fish
end

# Initialize starship async but outside interactive block
function __async_starship --on-event fish_prompt
    functions -e __async_starship
    starship init fish | source
end

# pnpm
set -gx PNPM_HOME "/home/leniviy/.local/share/pnpm"
if not string match -q -- $PNPM_HOME $PATH
  set -gx PATH "$PNPM_HOME" $PATH
end
# pnpm end
