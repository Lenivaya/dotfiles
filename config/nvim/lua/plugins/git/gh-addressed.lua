return {
  "dlvhdr/gh-addressed.nvim",
  event = "VeryLazy",
  dependencies = {
    "nvim-lua/plenary.nvim",
    "MunifTanjim/nui.nvim",
    "folke/trouble.nvim",
  },
  cmd = "GhReviewComments",
  keys = {
    { "<leader>gR", "<cmd>GhReviewComments<cr>", desc = "GitHub Review Comments" },
  },
}
