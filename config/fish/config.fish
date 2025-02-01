set fish_function_path $fish_function_path ~/plugin-foreign-env/functions

if status is-interactive
    # Commands to run in interactive sessions can go here
    atuin init fish | source
    zoxide init fish | source
    nix-your-shell fish | source
end
starship init fish | source
