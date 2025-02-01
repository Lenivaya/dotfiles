complete nix-instantiate -l add-root
complete nix-instantiate -l indirect
complete nix-instantiate -l parse -d "Just parse the input files and print their abstract syntax trees"
complete nix-instantiate -l eval -d "Just parse and evaluate the input files, and print the resulting values"
complete nix-instantiate -l find-file -d "Look up the given files on Nix's search path"
complete nix-instantiate -l strict -d "Cause --eval to recursively evaluate list elements and attributes"
complete nix-instantiate -l json -d "Print output from --eval as JSON"
complete nix-instantiate -l xml -d "Print output from --eval as XML"
complete nix-instantiate -l read-write-mode -d "Perform evaluation in read/write mode"

_nix_complete_common_options nix-instantiate
