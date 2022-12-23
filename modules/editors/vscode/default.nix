{
  config,
  options,
  pkgs,
  lib,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.editors.vscode;
  inherit (config.dotfiles) configDir;
in {
  options.modules.editors.vscode = with types; {
    enable = mkBoolOpt false;
  };

  # Min tomorrow night theme
  config = mkIf cfg.enable {
    # HACK to make config writable
    home.activation.boforeCheckLinkTargets = {
      after = [];
      before = ["checkLinkTargets"];
      data = ''
        userDir=~/.config/Code/User
        rm -rf $userDir/settings.json
        rm -rf $userDir/keybindings.json
      '';
    };

    home.activation.afterWriteBoundary = {
      after = ["writeBoundary"];
      before = [];
      data = ''
        userDir=~/.config/Code/User
        rm -rf $userDir/settings.json
        rm -rf $userDir/keybindings.json
        cat \
          ${(pkgs.formats.json {}).generate "blabla"
          config.home.programs.vscode.userSettings} \
          > $userDir/settings.json
        cat \
          ${(pkgs.formats.json {}).generate "blabla"
          config.home.programs.vscode.keybindings} \
          > $userDir/keybindings.json
      '';
    };

    home.programs.vscode = {
      enable = true;
      mutableExtensionsDir = true;
      userSettings = import "${configDir}/vscode/settings.nix" {
        inherit pkgs;
        inherit config;
      };
      keybindings = import "${configDir}/vscode/keybindings.nix";
      extensions = with pkgs.vscode-extensions;
        [
          vscodevim.vim
          bodil.file-browser

          editorconfig.editorconfig
          codezombiech.gitignore
          mikestead.dotenv
          shd101wyy.markdown-preview-enhanced
          yzhang.markdown-all-in-one

          ms-vscode.cpptools
          ms-python.python
          # matklad.rust-analyzer

          dbaeumer.vscode-eslint
          esbenp.prettier-vscode
          davidanson.vscode-markdownlint

          jnoortheen.nix-ide
          kamadorueda.alejandra

          timonwong.shellcheck

          WakaTime.vscode-wakatime
          alefragnani.project-manager
          # tomoki1207.pdf

          # kahole.magit
          vincaslt.highlight-matching-tag
          spywhere.guides
          vspacecode.whichkey
          # usernamehw.errorlens
        ]
        ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
          {
            name = "better-comments";
            publisher = "aaron-bond";
            version = "3.0.2";
            sha256 = "15w1ixvp6vn9ng6mmcmv9ch0ngx8m85i1yabxdfn6zx3ypq802c5";
          }

          {
            name = "Bookmarks";
            publisher = "alefragnani";
            version = "13.3.1";
            sha256 = "0q3zc84f4dl9zhsr6j569bm4jgyjlrzn9zf3fcpw871wnak8b509";
          }

          {
            name = "vscode-sqlite";
            publisher = "alexcvzz";
            version = "0.14.1";
            sha256 = "1iaklnhw74iwyjw74prnrx34ba25ra7ld71zlip04lv401329r4c";
          }

          {
            name = "vscode-bazel";
            publisher = "BazelBuild";
            version = "0.5.0";
            sha256 = "0gjf42xjhzwbncd6c8p7c60m44bkhk2kcpa3qjg2vr619p0i5514";
          }

          {
            name = "solargraph";
            publisher = "castwide";
            version = "0.23.0";
            sha256 = "0ivawyq16712j2q4wic3y42lbqfml5gs24glvlglpi0kcgnii96n";
          }

          {
            name = "path-intellisense";
            publisher = "christian-kohler";
            version = "2.8.1";
            sha256 = "1j7q4mzj173sl6xl3zjw40hnqvyqsrsczakmv63066k4k0rb6clm";
          }

          {
            name = "doxdocgen";
            publisher = "cschlosser";
            version = "1.4.0";
            sha256 = "1d95znf2vsdzv9jqiigh9zm62dp4m9jz3qcfaxn0n0pvalbiyw92";
          }

          {
            name = "githistory";
            publisher = "donjayamanne";
            version = "0.6.19";
            sha256 = "15s2mva9hg2pw499g890v3jycncdps2dmmrmrkj3rns8fkhjn8b3";
          }

          {
            name = "gitlens";
            publisher = "eamodio";
            version = "12.1.2";
            sha256 = "0wpmfrfpi6wl9v3dknx2qr2m74azpcw8bvhac21v67w6jxnl3jd9";
          }

          # {
          #   name = "vsc-material-theme";
          #   publisher = "Equinusocio";
          #   version = "33.5.0";
          #   sha256 = "1pr98mx7hji8jlm6ppac693ivbcpybh043w2z8sa3f49v7pksnrf";
          # }

          # {
          #   name = "vsc-material-theme-icons";
          #   publisher = "equinusocio";
          #   version = "2.3.1";
          #   sha256 = "1djm4k3hcn4aq63d4mxs2n4ffq5x1qr82q6gxwi5pmabrb0hrb30";
          # }

          {
            name = "min-tomorrow-theme";
            publisher = "mustafamohamad";
            version = "1.5.1";
            sha256 = "sha256-Aoo+Gxdtz1G01lrOKJ+kV6un6HKhI/AlRcKe6CwLQwI=";
          }

          {
            name = "auto-rename-tag";
            publisher = "formulahendry";
            version = "0.1.10";
            sha256 = "0nyilwfs2kbf8v3v9njx1s7ppdp1472yhimiaja0c3v7piwrcymr";
          }

          {
            name = "copilot";
            publisher = "GitHub";
            version = "1.39.6432";
            sha256 = "073kbjnf44hzy2n6zf5dns2ps9p8r929mg6az11darza3nk98vbn";
          }

          {
            name = "vscode-pull-request-github";
            publisher = "GitHub";
            version = "0.49.2022080912";
            sha256 = "10697s0q3skizj48i2ng8190gci94rdvnjqcq9q306kjkhqmy04d";
          }

          {
            name = "go";
            publisher = "golang";
            version = "0.35.1";
            sha256 = "0n7jzns44sv8xzkvs3bp753605gv3sps0l16br8cm4cj38bjnx1h";
          }

          {
            name = "todo-tree";
            publisher = "Gruntfuggly";
            version = "0.0.215";
            sha256 = "0lyaijsvi1gqidpn8mnnfc0qsnd7an8qg5p2m7l24c767gllkbsq";
          }

          {
            name = "haskell";
            publisher = "haskell";
            version = "2.2.0";
            sha256 = "0qgp93m5d5kz7bxlnvlshcd8ms5ag48nk5hb37x02giqcavg4qv0";
          }

          {
            name = "latex-workshop";
            publisher = "James-Yu";
            version = "8.28.0";
            sha256 = "1vxxwk0disbxg48w119lh408i4kkqvcrh4q1x5gxqa12pvzafzb4";
          }

          {
            name = "better-cpp-syntax";
            publisher = "jeff-hykin";
            version = "1.15.19";
            sha256 = "13v1lqqfvgkf5nm89b39hci65fnz4j89ngkg9p103l1p1fhncr41";
          }

          # {
          #   name = "nix-ide";
          #   publisher = "jnoortheen";
          #   version = "0.1.20";
          #   sha256 = "16mmivdssjky11gmih7zp99d41m09r0ii43n17d4i6xwivagi9a3";
          # }

          {
            name = "lean";
            publisher = "jroesch";
            version = "0.16.53";
            sha256 = "17l36nc14f6y5mx71s10wq2dzdy5phksqqrgrc20la88bcyrf7xk";
          }

          # {
          #   name = "language-julia";
          #   publisher = "julialang";
          #   version = "1.6.30";
          #   sha256 = "0i31nhpcmbwy8zka5y8zspda42r2v8i0g0pi920s8w1lr6ssb5hx";
          # }

          {
            name = "language-haskell";
            publisher = "justusadam";
            version = "3.6.0";
            sha256 = "115y86w6n2bi33g1xh6ipz92jz5797d3d00mr4k8dv5fz76d35dd";
          }

          {
            name = "magit";
            publisher = "kahole";
            version = "0.6.30";
            sha256 = "0s4vn7n4kyy4v3jkv10xw348p9d7z8dfzknq4gninc1phwyb3pg3";
          }

          {
            name = "mips";
            publisher = "kdarkhan";
            version = "0.1.0";
            sha256 = "00i853yshgx9m8kprv6y5swz8i7rqqbvz9wd2dlnvfc4nsbn9y6m";
          }

          {
            name = "noctis";
            publisher = "liviuschera";
            version = "10.40.0";
            sha256 = "1ry0vkyb92c6p6i8dpjq7sihvbpl45gngb8fym22nylmnfi9dcai";
          }

          {
            name = "vscode-language-babel";
            publisher = "mgmcdermott";
            version = "0.0.36";
            sha256 = "0v2xqry7pgwnzxi534v2rrbkfz9gvvbyc2px2g0xpbaaz3rrz6kd";
          }

          {
            name = "git-graph";
            publisher = "mhutchie";
            version = "1.30.0";
            sha256 = "000zhgzijf3h6abhv4p3cz99ykj6489wfn81j0s691prr8q9lxxh";
          }

          {
            name = "vscode-clang";
            publisher = "mitaki28";
            version = "0.2.4";
            sha256 = "0sys2h4jvnannlk2q02lprc2ss9nkgh0f0kwa188i7viaprpnx23";
          }

          {
            name = "vscode-filesize";
            publisher = "mkxml";
            version = "3.1.0";
            sha256 = "1zxdsqr5h0xl6arphi5i1xfgby4cin39jxpnmdgcg41p6qr3k3z7";
          }

          {
            name = "vscode-docker";
            publisher = "ms-azuretools";
            version = "1.22.1";
            sha256 = "1ix363fjxi9g450rs3ghx44z3hppvasf0xpzgha93m90djd7ai52";
          }

          {
            name = "vscode-pylance";
            publisher = "ms-python";
            version = "2022.8.12";
            sha256 = "1bqphr39bnypxkwxcgb0gand1818p10brckl7pqkgw1c6bjs0zmg";
          }

          {
            name = "jupyter";
            publisher = "ms-toolsai";
            version = "2022.8.1002231050";
            sha256 = "1xmbd839i8g4x1fk2n3b0fffflhrrmnax01xf54was735pli9vic";
          }

          {
            name = "remote-containers";
            publisher = "ms-vscode-remote";
            version = "0.244.0";
            sha256 = "0m6rf4m30k92xs59mwdv68pam949qab2ai7mnvkwbd7kspfb0nlg";
          }

          {
            name = "remote-ssh";
            publisher = "ms-vscode-remote";
            version = "0.85.2022071315";
            sha256 = "195c22wvvw727li8wf0nhfpccgynx9sd48gzz39j8wxmi7qqvjli";
          }

          {
            name = "azure-account";
            publisher = "ms-vscode";
            version = "0.11.1";
            sha256 = "051cfhgvz6mkmp5i7f4fv7kykbiwhqcc3hswsbw93jqh8l33qnym";
          }

          {
            name = "cmake-tools";
            publisher = "ms-vscode";
            version = "1.12.20";
            sha256 = "1j2lavrlhn5xxd7gvxgx6i3kjlm202w5z2nbikl1xs2jc9s1lgzw";
          }

          {
            name = "hexeditor";
            publisher = "ms-vscode";
            version = "1.9.8";
            sha256 = "063n4plhbjm6l5gip6j158n6hgydiccq1f8rc1pgsbfjn3d4612y";
          }

          {
            name = "vs-keybindings";
            publisher = "ms-vscode";
            version = "0.2.1";
            sha256 = "1h7dihd6f39jcp27haiwbjdsymyi5p2v4f101lxdi5fafz3y6win";
          }

          {
            name = "vsliveshare";
            publisher = "ms-vsliveshare";
            version = "1.0.5683";
            sha256 = "0m6idwbynfvqilvhnkdyiyi207vq50m6cbfi7pfwqb0b5c64771x";
          }

          {
            name = "gremlins";
            publisher = "nhoizey";
            version = "0.26.0";
            sha256 = "1sfs98nxm5ylcjrmylr5y68ddml8cfg1q1wdm7wvhfhjqx4kig9h";
          }

          {
            name = "explorer-exclude";
            publisher = "PeterSchmalfeldt";
            version = "1.3.1";
            sha256 = "0w97dih5i750qkp4addzb3474h1xhshlc0wqmx0lxjw6fmgdnc8x";
          }

          {
            name = "quicktype";
            publisher = "quicktype";
            version = "12.0.46";
            sha256 = "0mzn1favvrzqcigr74gmy167qak5saskhwcvhf7f00z7x0378dim";
          }

          # {
          #   name = "ruby";
          #   publisher = "rebornix";
          #   version = "0.28.1";
          #   sha256 = "179g7nc6mf5rkha75v7rmb3vl8x4zc6qk1m0wn4pgylkxnzis18w";
          # }

          # {
          #   name = "java";
          #   publisher = "redhat";
          #   version = "1.10.2022080505";
          #   sha256 = "sha256-weLLc1603bWIu5g4Qis/2RM8fZ7vRz1hTPJK+K7eaNs=";
          # }

          {
            name = "r";
            publisher = "REditorSupport";
            version = "2.5.2";
            sha256 = "16lla2i0wwvc3y73ii3s6yffgpqmvx1qy9iygwbg5rs13abqnsd8";
          }

          {
            name = "rust-analyzer";
            publisher = "rust-lang";
            version = "0.4.1164";
            sha256 = "sha256-YBxj0z8JA4jyth9Kt77yCXZZU1T2tZljslVIdxpp+4c=";
          }

          {
            name = "jinjahtml";
            publisher = "samuelcolvin";
            version = "0.17.0";
            sha256 = "120z8barzgva0sr1g7xj4arpjz96v4zxh2zgk56jzdgnafzyq71b";
          }

          {
            name = "scala";
            publisher = "scala-lang";
            version = "0.5.6";
            sha256 = "004zc3id5jk8hk6q27g4p36prvnlqdsgda0gd6xvs4gamhywhb3s";
          }

          {
            name = "shader";
            publisher = "slevesque";
            version = "1.1.5";
            sha256 = "14yraymi96f6lpcrwk93llbiraq8gqzk7jzyw7xmndhcwhazpz9x";
          }

          {
            name = "vscode-stylelint";
            publisher = "stylelint";
            version = "1.2.2";
            sha256 = "00v31vsp6nnw6zvv6a854cvzh63y9l712z57hh7na4x9if9pk9bg";
          }

          {
            name = "cmake";
            publisher = "twxs";
            version = "0.0.17";
            sha256 = "11hzjd0gxkq37689rrr2aszxng5l9fwpgs9nnglq3zhfa1msyn08";
          }

          {
            name = "errorlens";
            publisher = "usernamehw";
            version = "3.6.0";
            sha256 = "1sv8vlzmynbz20vmv901nrg12wcmsg5i9pm6mqq32rlgb7rw3p50";
          }

          # {
          #   name = "vscode-java-debug";
          #   publisher = "vscjava";
          #   version = "0.43.2022080507";
          #   sha256 = "1s1ksy5dn4dckn4kbyb3nx5mgchrx76i7gs4kmc3zypnwgjzvqia";
          # }

          # {
          #   name = "vscode-java-pack";
          #   publisher = "vscjava";
          #   version = "0.25.2022072300";
          #   sha256 = "0ja108nzmgm081phb8nhrgsm8w5pl51cpz797pi2ld58z7xy9qh5";
          # }

          # {
          #   name = "vscode-java-test";
          #   publisher = "vscjava";
          #   version = "0.36.2022080202";
          #   sha256 = "1jz2w4w9d79ppmwm66c2g8kkxra9ay1hn28w5ahrwj0frwgkplzl";
          # }

          # {
          #   name = "vscode-maven";
          #   publisher = "vscjava";
          #   version = "0.37.2022072603";
          #   sha256 = "1dxjjprn2s3mq2wybaz90fa4rx429v2ppnv5yg6ana4r8hmjyh1r";
          # }
        ];
    };
    # user.packages = with pkgs; [
    #   vscode
    # ];
  };
}
