return {
  {
    "nvim-treesitter/nvim-treesitter",
    opts = { ensure_installed = { "graphql" } },
  },
  {
    "neovim/nvim-lspconfig",
    ---@class PluginLspOpts
    opts = {
      ---@module 'lspconfig'
      ---@type {[string]: lspconfig.Config|{}}
      servers = { graphql = {} },
    },
  },
}
