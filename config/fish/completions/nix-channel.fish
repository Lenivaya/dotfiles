complete nix-channel --exclusive

complete nix-channel -n __fish_is_first_arg -a --add -d "Subscribe to a channel"
complete nix-channel -n __fish_is_first_arg -a --remove -d "Unsubscribe from a channel"
complete nix-channel -n __fish_is_first_arg -a --list -d "List subscribed channels"
complete nix-channel -n __fish_is_first_arg -a --update -d "Update and activate channels"
complete nix-channel -n __fish_is_first_arg -a --rollback -d "Revert the previous nix-channel --update"

complete nix-channel -n "__fish_seen_argument -l remove -l update" -a "(nix-channel --list | string split --fields 1 ' ')"
