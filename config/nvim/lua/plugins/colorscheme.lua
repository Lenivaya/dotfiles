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
      flavor = "mocha",
      no_italic = false,
      term_colors = false,
      transparent_background = true,
      color_overrides = {
        mocha = {
          base = "#000000",
          mantle = "#000000",
          crust = "#000000",
        },
      },
    },
  },
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

  --
  -- {
  --   "craftzdog/solarized-osaka.nvim",
  --   lazy = false,
  --   priority = 1000,
  --   opts = {},
  --   config = function()
  --     require("solarized-osaka").setup({
  --       -- your configuration comes here
  --       -- or leave it empty to use the default settings
  --       transparent = false, -- Enable this to disable setting the background color
  --       terminal_colors = false, -- Configure the colors used when opening a `:terminal` in [Neovim](https://github.com/neovim/neovim)
  --       -- styles = {
  --       --   -- Style to be applied to different syntax groups
  --       --   -- Value is any valid attr-list value for `:help nvim_set_hl`
  --       --   comments = { italic = true },
  --       --   keywords = { italic = true },
  --       --   functions = {},
  --       --   variables = {},
  --       --   -- Background styles. Can be "dark", "transparent" or "normal"
  --       --   sidebars = "dark", -- style for sidebars, see below
  --       --   floats = "dark", -- style for floating windows
  --       -- },
  --       -- sidebars = { "qf", "help" }, -- Set a darker background on sidebar-like windows. For example: `["qf", "vista_kind", "terminal", "packer"]`
  --       -- day_brightness = 0.3, -- Adjusts the brightness of the colors of the **Day** style. Number between 0 and 1, from dull to vibrant colors
  --       hide_inactive_statusline = false, -- Enabling this option, will hide inactive statuslines and replace them with a thin border instead. Should work with the standard **StatusLine** and **LuaLine**.
  --       dim_inactive = true, -- dims inactive windows
  --       -- lualine_bold = false, -- When `true`, section headers in the lualine theme will be bold
  --
  --       --- You can override specific color groups to use other groups or a hex color
  --       --- function will be called with a ColorScheme table
  --       ---@param colors ColorScheme
  --       -- on_colors = function(colors) end,
  --
  --       --- You can override specific highlights to use other groups or a hex color
  --       --- function will be called with a Highlights and ColorScheme table
  --       ---@param highlights Highlights
  --       ---@param colors ColorScheme
  --       -- on_highlights = function(highlights, colors) end,
  --     })
  --     require("solarized-osaka").load()
  --   end,
  -- },
  --
}
