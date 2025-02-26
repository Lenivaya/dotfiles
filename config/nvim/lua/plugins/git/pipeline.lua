-- ci/cd things in neovim
return {
  "topaxi/pipeline.nvim",
  keys = {
    { "<leader>ci", "<cmd>Pipeline<cr>", desc = "Open pipeline.nvim" },
  },
  -- optional, you can also install and use `yq` instead.
  build = "make",
  ---@type pipeline.Config
  opts = {},
}
