return {
  { "fei6409/log-highlight.nvim", event = "BufRead *.log", opts = {} },
  {
    "nvim-treesitter/nvim-treesitter",
    opts = {
      ensure_installed = {
        "astro",
        "typescript",
        "javascript",
        "jsdoc",
        "cmake",
        "cpp",
        "css",
        "fish",
        "gitignore",
        "gitcommit",
        "git_config",
        "git_rebase",
        "gitattributes",
        "go",
        "graphql",
        "http",
        "query",
        "scheme",
        "java",
        "php",
        "rust",
        "scss",
        "sql",
        "svelte",
        "devicetree",
        "nix",
        "org",
        "vue",
        "just",
        "haskell",
        "xml",
        "xresources",
        "sxhkdrc",
        "solidity",
        "dockerfile",
        "bash",
        "json",
        "jsonc",
        "json5",
        "lua",
        "prisma",
        "vimdoc",
        "vim",
        "regex",
      },

      -- matchup = {
      -- 	enable = true,
      -- },
    },
    config = function(_, opts)
      require("nvim-treesitter.configs").setup(opts)

      -- MDX
      vim.filetype.add({
        extension = {
          mdx = "mdx",
        },
      })
      vim.treesitter.language.register("markdown", "mdx")
    end,
  },
}
