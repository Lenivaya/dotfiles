complete nix-env --exclusive

# Common options
complete nix-env -l file -s f -d "Specify Nix expression used to obtain derivations"
complete nix-env -l profile -s p -d "Specify the profile to use"
complete nix-env -l dry-run
complete nix-env -l system-filter -d "Only show derivations matching the specified platform"

_nix_complete_common_options nix-env

# Operation --install
complete nix-env -n "__fish_not_contain_opt install upgrade uninstall set set-flag query switch-profile list-generations delete-generations switch-generation rollback -s i -s u -s e -s q -s S -s G" -a "--install -i" -d "Install package"
complete nix-env -n "__fish_seen_argument -l install -s i" -l prebuilt-only -s b -d "Fail if there is no pre-built binary available"
complete nix-env -n "__fish_seen_argument -l install -s i" -l from-expressions
complete nix-env -n "__fish_seen_argument -l install -s i" -l from-profile -d "Fetch store paths from another profile"
complete nix-env -n "__fish_seen_argument -l install -s i" -l preserve-installed -s P -d "Do not remove derivations with the same name"
complete nix-env -n "__fish_seen_argument -l install -s i" -l remove-all -s r -d "Remove all previously installed packages prior to installing"

# Operation --upgrade
complete nix-env -n "__fish_not_contain_opt install upgrade uninstall set set-flag query switch-profile list-generations delete-generations switch-generation rollback -s i -s u -s e -s q -s S -s G" -a "--upgrade -u" -d "Upgrade package"
complete nix-env -n "__fish_seen_argument -l upgrade -s u" -l prebuilt-only -s b -d "Fail if there is no pre-built binary available"
complete nix-env -n "__fish_seen_argument -l upgrade -s u" -l from-expressions
complete nix-env -n "__fish_seen_argument -l upgrade -s u" -l from-profile -d "Fetch store paths from another profile"
complete nix-env -n "__fish_seen_argument -l upgrade -s u" -l lt -d "Upgrade derivations with newer versions (default)"
complete nix-env -n "__fish_seen_argument -l upgrade -s u" -l leq -d "Upgrade derivations with the same or newer version"
complete nix-env -n "__fish_seen_argument -l upgrade -s u" -l eq -d "Upgrade derivations with equivalent versions"
complete nix-env -n "__fish_seen_argument -l upgrade -s u" -l always -d "Upgrade even if version number decreases"

# Operation --uninstall
complete nix-env -n "__fish_not_contain_opt install upgrade uninstall set set-flag query switch-profile list-generations delete-generations switch-generation rollback -s i -s u -s e -s q -s S -s G" -a "--uninstall -e" -d "Uninstall package"

# Operation --set
complete nix-env -n "__fish_not_contain_opt install upgrade uninstall set set-flag query switch-profile list-generations delete-generations switch-generation rollback -s i -s u -s e -s q -s S -s G" -a --set -d "Modify profile to only contain specified derivation"

# Operation --set-flag
complete nix-env -n "__fish_not_contain_opt install upgrade uninstall set set-flag query switch-profile list-generations delete-generations switch-generation rollback -s i -s u -s e -s q -s S -s G" -a --set-flag -d "Modify meta attribute of installed package"

# Operation --query
complete nix-env -n "__fish_not_contain_opt install upgrade uninstall set set-flag query switch-profile list-generations delete-generations switch-generation rollback -s i -s u -s e -s q -s S -s G" -a "--query -q" -d "List information about derivations"
complete nix-env -n "__fish_seen_argument -l query -s q" -l installed
complete nix-env -n "__fish_seen_argument -l query -s q" -l available -s a -d "Display all installable derivations"
complete nix-env -n "__fish_seen_argument -l query -s q" -l status -s s -d "Print status of derivation"
complete nix-env -n "__fish_seen_argument -l query -s q" -l attr-path -s P -d "Print attribute path of derivations"
complete nix-env -n "__fish_seen_argument -l query -s q" -l no-name -d "Suppress printing of name attribute"
complete nix-env -n "__fish_seen_argument -l query -s q" -l compare-versions -s c -d "Compare installed and available version"
complete nix-env -n "__fish_seen_argument -l query -s q" -l system -d "Print system attribute"
complete nix-env -n "__fish_seen_argument -l query -s q" -l drv-path -d "Print store derivation path"
complete nix-env -n "__fish_seen_argument -l query -s q" -l out-path -d "Print output path"
complete nix-env -n "__fish_seen_argument -l query -s q" -l description -d "Print description"
complete nix-env -n "__fish_seen_argument -l query -s q" -l meta -d "Print all meta attributes (only available with --xml)"
complete nix-env -n "__fish_seen_argument -l query -s q" -l xml -d "Print output as xml"
complete nix-env -n "__fish_seen_argument -l query -s q" -l json -d "Print output as json"
complete nix-env -n "__fish_seen_argument -l query -s q" -l prebuilt-only -s b -d "Fail if there is no pre-built binary available"

# Operation --switch-profile
complete nix-env -n "__fish_not_contain_opt install upgrade uninstall set set-flag query switch-profile list-generations delete-generations switch-generation rollback -s i -s u -s e -s q -s S -s G" -a "--switch-profile -S" -d "Set the current profile path"

# Operation --list-generations
complete nix-env -n "__fish_not_contain_opt install upgrade uninstall set set-flag query switch-profile list-generations delete-generations switch-generation rollback -s i -s u -s e -s q -s S -s G" -a --list-generations -d "Print a list of all generations in the active profile"

# Operation --delete-generations
complete nix-env -n "__fish_not_contain_opt install upgrade uninstall set set-flag query switch-profile list-generations delete-generations switch-generation rollback -s i -s u -s e -s q -s S -s G" -a --delete-generations -d "Delete specified generations"

# Operation --switch-generation
complete nix-env -n "__fish_not_contain_opt install upgrade uninstall set set-flag query switch-profile list-generations delete-generations switch-generation rollback -s i -s u -s e -s q -s S -s G" -a "--switch-generation -G" -d "Activate specified generation"

# Operation --rollback
complete nix-env -n "__fish_not_contain_opt install upgrade uninstall set set-flag query switch-profile list-generations delete-generations switch-generation rollback -s i -s u -s e -s q -s S -s G" -a --rollback -d "Switch to the previous generation of active profile"

complete nix-env -n "__fish_seen_argument -l delete-generations -l switch-generation -s G" -a "(nix-env --list-generations | string split --fields 2 ' ')"
