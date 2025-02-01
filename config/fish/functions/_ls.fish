# `eza` wrapper with parameter handling
function _ls
    if set -q eza_params
        eza $argv $eza_params
    else
        set -lx params --git \
            --icons \
            --group \
            --group-directories-first \
            --time-style=long-iso \
            --color-scale=all

        eza $argv $params
    end
end
