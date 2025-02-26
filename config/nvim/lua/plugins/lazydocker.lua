return {
  "crnvl96/lazydocker.nvim",
  event = "VeryLazy",
  opts = {}, -- automatically calls `require("lazydocker").setup()`
  dependencies = {
    "MunifTanjim/nui.nvim",
  },
  keys = {
    { "<leader>kk", "<cmd>LazyDocker<cr>", desc = "Open lazydocker" },
  },
}
