function __dl_print_help
    echo "Usage: dl [OPTION] URL"
    echo
    echo "Description:"
    echo "    Easily download songs to your Music directory."
    echo "    Currently limited to audio (MP3) only."
    echo "    Depends on yt-dlp."
    echo
    echo "Examples:"
    echo "    dl 'https://www.youtube.com/watch?v=some-video'"
    echo "    dl https://soundcloud.com/some-profile/some-song"
    echo
    echo "Options:"
    echo "    -h, --help      Prints helps"
    echo "    -v, --version   Prints version"
end

function __dl_print_version
    echo "dl version 1.2.5"
end

function dl --description "Opinionated alias for yt-dlp"
    set --local options 'h/help' 'v/version'
    argparse $options -- $argv

    if set --query _flag_help
        __dl_print_help
        return 0
    else if set --query _flag_version
        __dl_print_version
        return 0
    end

    for path in $argv
        __dl_download $path
    end
end

function __dl_download --argument path
    set --local output_dir "$HOME/Music"
    set --local yt_dl_options "--ignore-config" \
                              "--quiet" \
                              "--no-mtime" \
                              "--output '$output_dir/%(title)s.%(ext)s'" \
                              "--extract-audio" \
                              "--audio-format mp3"
    set --local dl_cmd "yt-dlp $yt_dl_options '$path'"
    set --local spinner_msg " @ Saving to $output_dir\r"

    spin --format $spinner_msg $dl_cmd
end
