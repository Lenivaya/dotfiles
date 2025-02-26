return {
  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = "catppuccin",
    },
  },
  {
    "catppuccin/nvim",
    name = "catppuccin",
    priority = 1000,
    version = false, -- always use the latest git commit
    opts = {
      -- https://github.com/catppuccin/nvim#integrations
      integrations = {
        hop = true,
        mason = true,
        grug_far = true,
        cmp = true,
        blink_cmp = true,
        noice = true,
        gitsigns = true,
        treesitter_context = true,
        copilot_vim = true,
        ufo = true,
        octo = true,
        window_picker = true,
        neotest = true,
        treesitter = true,
        gitgraph = true,
        flash = true,
        avante = true,
        snacks = true,
        gitgutter = true,
      },
      default_integrations = true,
      no_italic = false,
      no_bold = false,
      no_underline = false,
      term_colors = false,
      show_end_of_buffer = false,
      transparent_background = true,
      flavor = "mocha",
      color_overrides = {
        mocha = {
          base = "#000000",
          mantle = "#000000",
          crust = "#000000",
        },
      },

      styles = {
        comments = { "italic" },
        keywords = { "italic" },
        conditionals = { "italic" },
      },
    },
  },
  --
  --
  -- {
  --   "LazyVim/LazyVim",
  --   opts = {
  --     colorscheme = "gruvbox-material",
  --   },
  -- },
  -- {
  --   "f4z3r/gruvbox-material.nvim",
  --   name = "gruvbox-material",
  --   lazy = false,
  --   priority = 1000,
  --   opts = {
  --     background = {
  --       transparent = true,
  --     },
  --   },
  -- },
  --
  --
  -- {
  --   "dgox16/oldworld.nvim",
  --   lazy = false,
  --   priority = 1000,
  --   opts = {
  --     highlight_overrides = {
  --       Normal = { bg = "NONE" },
  --       NormalNC = { bg = "NONE" },
  --       CursorLine = { bg = "#222128" },
  --     },
  --     integrations = { -- You can disable/enable integrations
  --       alpha = true,
  --       cmp = true,
  --       flash = true,
  --       gitsigns = true,
  --       hop = true,
  --       indent_blankline = true,
  --       lazy = true,
  --       lsp = true,
  --       markdown = true,
  --       mason = true,
  --       navic = true,
  --       neo_tree = true,
  --       neogit = true,
  --       neorg = true,
  --       noice = true,
  --       notify = true,
  --       rainbow_delimiters = true,
  --       telescope = true,
  --       treesitter = true,
  --     },
  --   },
  -- },
  -- {
  --   "deparr/tairiki.nvim",
  --   lazy = false,
  --   -- priority = 1000, -- only necessary if you use tairiki as default theme
  --   branch = "v2",
  --   version = false, -- always use the latest git commit
  --   config = function()
  --     require("tairiki").setup({
  --       -- plugins = {
  --       --   auto = true,
  --       -- },
  --
  --       palette = "dark",
  --       terminal = true,
  --       -- -- optional configuration here
  --       term_colors = true,
  --       transparent = true,
  --       -- end_of_buffer = true,
  --       -- visual_bold = true,
  --     })
  --     require("tairiki").load() -- only necessary to use as default theme, has same behavior as ':colorscheme tairiki'
  --   end,
  -- },
}
