function web-search -d "Search on the web"
    _web_search_load_custom_engines

    set -l url (_web_search_get_url $argv)
    if test $status -eq 1
        echo "'$argv[1]' is not supported."
        return 1
    end

    set -l open_cmd (_web_search_get_open_cmd)
    eval "$open_cmd '$url' &> /dev/null & disown"
end
