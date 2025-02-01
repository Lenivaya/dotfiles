# package manager detection logic is based on [kKaribash/ni.fish]
# Original Source: https://github.com/Karibash/ni.fish/blob/main/functions/ni.fish

# Set the fuzzy finder command to use
if test -z "$NA_FUZZYFINDER"
    set -g NA_FUZZYFINDER fzf
end

# Set the fuzzy finder options
if test -z "$NA_FUZZYFINDER_OPTIONS"
    set -g NA_FUZZYFINDER_OPTIONS
end

# Set your favorite package manager list. You can customize the order.
if test -z "$NA_PACKAGE_MANAGER_LIST"
    set -g NA_PACKAGE_MANAGER_LIST npm pnpm bun yarn deno
end

function _na
    _na_get_package_manager_name $PWD
end

function _na_find_up --argument-names path
    set files $argv[2..]
    for file in $files
        test -e "$path/$file" && echo $path/$file && return
    end

    test ! -z "$path" || return
    _na_find_up (string replace --regex -- '/[^/]*$' "" $path) $files
end

function _na_find_lock_file --argument-names path
    _na_find_up $path deno.lock package-lock.json npm-shrinkwrap.json yarn.lock pnpm-lock.yaml bun.lockb bun.lock bunfig.toml
end

function _na_find_package_json --argument-names path
    _na_find_up $path package.json
end

function _na_find_deno_json --argument-names path
    _na_find_up $path deno.json deno.jsonc
end

function _na_select_package_manager_with_fuzzy_finder --argument-names path
    echo ( for i in $NA_PACKAGE_MANAGER_LIST; echo $i; end  | $NA_FUZZYFINDER $NA_FUZZYFINDER_OPTIONS )
end

function _na_get_package_manager_name --argument-names path
    set lock_file_path (_na_find_lock_file $path)
    set deno_json_path (_na_find_deno_json $path)
    set package_json_path (test -n "$lock_file_path" && _na_find_package_json $lock_file_path || _na_find_package_json $path)

    if test -n "$deno_json_path"
        echo deno
        return
    end

    if test -n "$package_json_path"
        set package_manager_name (cat $package_json_path | string match --entire -r "packageManager" | string replace -r '.*"packageManager": "' '' | string trim | string split @)[1]
        set valid_package_manager_name npm yarn pnpm bun
        if test -n "$package_manager_name"
            if contains $package_manager_name $valid_package_manager_name
                echo $package_manager_name
                return
            end

            echo "na: Unknown packageManager: \"$package_manager_name\"" >&2
            return 1
        end
    end

    if test -n "$lock_file_path"
        switch (basename $lock_file_path)
            case "deno.lock"
                echo deno
            case "bun.lockb" "bun.lock" "bunfig.toml"
                echo bun
            case "yarn.lock"
                echo yarn
            case "pnpm-lock.yaml"
                echo pnpm
            case "package-lock.json" "npm-shrinkwrap.json"
                echo npm
            case '*'
                echo "Unknown lock file: \"$lock_file_path\"" >&2
        end
        return
    end

    echo (_na_select_package_manager_with_fuzzy_finder $path)
end
