complete nix-prefetch-url --exclusive

complete nix-prefetch-url -l version
complete nix-prefetch-url -l type -d "Use the specified hash algorithm"
complete nix-prefetch-url -l print-path -d "Print the resulting store path"
complete nix-prefetch-url -l unpack -d "Unpack tarball/zip first"
complete nix-prefetch-url -l name -d "Override the resulting nix store filename"

complete nix-prefetch-url -n "__fish_seen_argument -l type" -a "md5 sha1 sha256 sha512"
