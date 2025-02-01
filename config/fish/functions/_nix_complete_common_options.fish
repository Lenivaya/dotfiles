function _nix_complete_common_options --argument-names cmd
    complete $cmd -l help
    complete $cmd -l version
    complete $cmd -l verbose -s v -d "Increase verbosity level"
    complete $cmd -l quiet
    complete $cmd -l no-build-output -s Q -d "Silence builder output"
    complete $cmd -l max-jobs -s j -d "Set the maximum number of build jobs"
    complete $cmd -l cores
    complete $cmd -l max-silent-time
    complete $cmd -l timeout
    complete $cmd -l keep-going -s k -d "Keep going in case of failed builds"
    complete $cmd -l keep-failed -s K -d "Keep temporary directory used by failed builds"
    complete $cmd -l fallback
    complete $cmd -l no-build-hook
    complete $cmd -l readonly-mode
    complete $cmd -l arg
    complete $cmd -l argstr
    complete $cmd -l attr -s A -d "Specify packages by attribute path instead of name"
    complete $cmd -l expr -s E -d "Interpret arguments as Nix expressions"
    complete $cmd -s I
    complete $cmd -l option
    complete $cmd -l repair
end
