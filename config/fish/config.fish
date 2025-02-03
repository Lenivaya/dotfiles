set fish_function_path $fish_function_path ~/plugin-foreign-env/functions

if status is-interactive
    # Commands to run in interactive sessions can go here
    function __async_init --on-event fish_prompt
        # Only run once
        functions -e __async_init

        # Initialize tools in background
        zoxide init fish | source
        atuin init fish | source
        nix-your-shell fish | source
        direnv hook fish | source
    end
end

# Initialize starship async but outside interactive block
function __async_starship --on-event fish_prompt
    functions -e __async_starship
    starship init fish | source
end
