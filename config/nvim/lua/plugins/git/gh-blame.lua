return {
  "dlvhdr/gh-blame.nvim",
  event = "VeryLazy",
  dependencies = { "nvim-lua/plenary.nvim", "MunifTanjim/nui.nvim" },
  keys = {
    { "<leader>gW", "<cmd>GhBlameCurrentLine<cr>", desc = "GitHub PR Blame Current Line" },
  },
}
