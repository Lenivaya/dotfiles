# access _web_search_fish_engines to get the list of engines
for i in (seq 1 2 (count $_web_search_fish_engines))
    set -l context $_web_search_fish_engines[$i]
    set -a contexts "$context"
end

for custom_env in (env | grep ^WEB_SEARCH)
    set -l context (string split -m 1 = $custom_env)[1]
    set -a contexts ( echo "$context" | string sub -s 12)
end

complete -c web-search -f -n __fish_use_subcommand -a "$contexts"
