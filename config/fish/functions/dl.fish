set -g apple_auto_add_path ~/Music/Music/Media.localized/Automatically\ Add\ to\ Music.localized/

function __dl_print_help
    echo "Usage: dl [OPTION] URL"
    echo
    echo "Description:"
    echo "    Download audio (MP3) from YouTube, SoundCloud, etc. to your Music folder"
    echo "    or directly into your Apple Music library for automatic import."
    echo
    echo "Options:"
    echo "    -h, --help      Show this help message"
    echo "    -v, --version   Show version information"
    echo "    -s, --silent    Suppress progress spinner"
    echo "    -a, --apple     Save to Apple Music auto-add folder:"
    echo "                    $apple_auto_add_path"
    echo
    echo "Examples:"
    echo "    dl https://youtube.com/watch?v=abc123"
    echo "    dl --apple --silent https://soundcloud.com/artist/track"
end

function __dl_print_version
    set -l dl_version "1.3.0"
    set -l ytdlp_version (yt-dlp --version 2>/dev/null; or echo "not installed")
    printf "dl version %s\nyt-dlp version %s\n" $dl_version $ytdlp_version
end

function dl --description "Download audio with yt-dlp, optionally into Apple Music"
    set --local options 'h/help' 'v/version' 's/silent' 'a/apple'
    argparse $options -- $argv; or return

    if set --query _flag_help
        __dl_print_help; return 0
    else if set --query _flag_version
        __dl_print_version; return 0
    end

    if not command -q yt-dlp
        echo "Error: yt-dlp is not installed" >&2
        echo "Install with `brew install yt-dlp`" >&2
        echo "or visit https://github.com/yt-dlp/yt-dlp?tab=readme-ov-file#installation for alternative methods." >&2
        return 1
    end

    set --local silent (set --query _flag_silent && echo 1 || echo 0)
    set --local apple  (set --query _flag_apple  && echo 1 || echo 0)

    for url in $argv
        __dl_download $url $silent $apple
    end
end

function __dl_download --argument-names url silent apple
    set --local output_dir "$HOME/Music"
    if test $apple -eq 1
        set output_dir $apple_auto_add_path
        if not test -d $output_dir
            printf "error: Apple Music auto-add directory does not exist:\n       %s\n" $output_dir >&2
            return 1
        end
    end

    set --local yt_dl_options "--ignore-config" \
                              "--quiet" \
                              "--no-mtime" \
                              "--embed-metadata" \
                              "--output '$output_dir/%(title)s.%(ext)s'" \
                              "--extract-audio" \
                              "--audio-format mp3"
    set --local dl_cmd "yt-dlp $yt_dl_options '$url'"

    if test $silent -eq 1
        eval $dl_cmd
    else
        set --local clear_line "\r\033[K"
        set --local spinner_msg " @ Saving to $output_dir\r"
        spin --interval=100 --format="$spinner_msg" "$dl_cmd" && printf $clear_line
    end
end
