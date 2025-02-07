return {
  {
    "vuki656/package-info.nvim",
    event = "BufRead package.json",
    opts = {},
  },
  {
    "Saghen/blink.cmp",
    optional = true,
    dependencies = {
      "saghen/blink.compat",
      {
        "David-Kunz/cmp-npm",
        event = "BufRead package.json",
        opts = {},
      },
    },
    opts = {
      sources = {
        default = { "npm" },
        providers = {
          npm = {
            name = "npm",
            module = "blink.compat.source",
          },
        },
      },
      completion = {
        menu = {
          border = "rounded",
          winhighlight = "Normal:BlinkCmpDoc,FloatBorder:BlinkCmpDocBorder,CursorLine:BlinkCmpDocCursorLine,Search:None",
        },
        documentation = {
          window = {
            border = "rounded",
          },
        },
      },
    },
  },
  -- Add JavaScript & friends to treesitter
  {
    "nvim-treesitter/nvim-treesitter",
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "css", "html", "javascript", "jsdoc", "scss" })
      end
    end,
  },
  {
    "dmmulroy/ts-error-translator.nvim",
    ft = { "typescript" },
    config = true,
  },
  {
    "windwp/nvim-ts-autotag",
    evnet = "BufReadPre",
    opts = {},
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
    },
  },
  {
    -- Neovim plugin to automatic change normal string to template string
    -- in JS like languages
    -- https://github.com/axelvc/template-string.nvim
    "axelvc/template-string.nvim",
    ft = {
      "html",
      "typescript",
      "javascript",
      "typescriptreact",
      "javascriptreact",
      "vue",
      "svelte",
      "python",
    },
    config = true,
  },
  {
    "williamboman/mason.nvim",
    opts = { ensure_installed = { "prettier", "prettierd" } },
  },
  -- import additional language modules from upstream
  { import = "lazyvim.plugins.extras.lang.json" },
  { import = "lazyvim.plugins.extras.lang.typescript" },
}
