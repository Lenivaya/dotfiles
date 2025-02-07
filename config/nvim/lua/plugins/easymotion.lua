return {
  "smoka7/hop.nvim",
  version = "*",
  event = "VeryLazy",
  opts = {
    keys = "etovxqpdygfblzhckisuran",
    case_insensitive = true,
    jump_on_sole_occurrence = true,
    uppercase_labels = true,
    multi_windows = true,
  },
  cmd = { "HopWord", "HopLine", "HopLineStart", "HopWordCurrentLine", "HopPattern" },
  keys = {
    { "<leader>jw", "<cmd>HopWord<cr>", desc = "Jump to word" },
    { "<leader>j<leader>", "<cmd>HopPattern<cr>", desc = "Jump to pattern" },
    { "<leader>jl", "<cmd>HopLineStart<cr>", desc = "Jump to line" },
    { "<C-;>", "<cmd>HopPattern<cr>" },
  },
}
