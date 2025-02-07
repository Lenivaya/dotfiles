return {
  "linrongbin16/gitlinker.nvim",
  event = "VeryLazy",
  dependencies = {
    "nvim-lua/plenary.nvim",
  },
  cmd = "GitLink",
  keys = {
    { "<leader>gy", "<cmd>GitLink default_branch<cr>", mode = { "n", "v" }, desc = "Yank git link" },
    { "<leader>gY", "<cmd>GitLink! default_branch<cr>", mode = { "n", "v" }, desc = "Open git link" },
  },
  opts = {},
}
