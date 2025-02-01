complete nix-copy-closure --exclusive

complete nix-copy-closure -l to -d "Copy the closure to the remote machine (default)"
complete nix-copy-closure -l from -d "Copy the closure from the remote machine"
complete nix-copy-closure -l gzip -d "Enable compression of the SSH connection"
complete nix-copy-closure -l include-outputs -d "Also copy outputs of store derivations included in the closure"
complete nix-copy-closure -l use-substitutes -s s -d "Download files from the binary cache if possible"
complete nix-copy-closure -s v
