# @halostatue/fish-go/conf.d/halostatue_fish_go.fish:v2.1.2

if command --query go
    contains -- (go env GOROOT)/bin $fish_user_paths
    or fish_add_path --prepend --move --path (go env GOROOT)/bin

    contains -- (go env GOPATH)/bin $fish_user_paths
    or fish_add_path --prepend --move --path (go env GOPATH)/bin
end

function _halostatue_fish_go_uninstall -e halostatue_fish_go_uninstall
end
