return {
  {
    "saghen/blink.cmp",
    dependencies = {
      "mikavilpas/blink-ripgrep.nvim",
      "moyiz/blink-emoji.nvim",
      {
        "Kaiser-Yang/blink-cmp-git",
        dependencies = { "nvim-lua/plenary.nvim" },
      },
    },
    opts = {
      sources = {
        default = {
          "ripgrep",
          "emoji",
          "git",
        },
        providers = {
          ripgrep = {
            module = "blink-ripgrep",
            name = "Ripgrep",
            opts = {
              prefix_min_len = 5,
              context_size = 5,
              max_filesize = "1M",
              additional_rg_options = {},
            },
          },
          emoji = {
            module = "blink-emoji",
            name = "Emoji",
            score_offset = -15,
          },
          git = {
            module = "blink-cmp-git",
            name = "Git",
            opts = {},
          },
        },
      },
      keymap = {
        preset = "enter",
        ["<Tab>"] = { "select_next", "snippet_forward", "fallback" },
        ["<S-Tab>"] = { "select_prev", "snippet_forward", "fallback" },
      },
      completion = {
        menu = {
          draw = {
            padding = { 1, 0 },
            columns = { { "label", "label_description", gap = 1 }, { "kind_icon", "kind" } },
            components = {
              kind_icon = {
                width = { fill = true },
              },
            },
          },
          border = "rounded",
          winhighlight = "Normal:BlinkCmpDoc,FloatBorder:BlinkCmpDocBorder,CursorLine:BlinkCmpDocCursorLine,Search:None",
        },
        documentation = {
          auto_show = true,
          window = {
            border = "rounded",
          },
        },
      },
    },
  },
}
