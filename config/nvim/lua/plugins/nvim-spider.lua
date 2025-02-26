return {
  "chrisgrieser/nvim-spider",
  vscode = true,
  event = "LazyFile",
  lazy = true,
  opts = {
    consistentOperatorPending = true,
  },
  keys = {
    {
      "e",
      "<cmd>lua require('spider').motion('e')<CR>",
      mode = { "n", "o", "x" },
      desc = "Move to start of next word",
    },
    {
      "w",
      "<cmd>lua require('spider').motion('w')<CR>",
      mode = { "n", "o", "x" },
      desc = "Move to end of word",
    },
    {
      "b",
      "<cmd>lua require('spider').motion('b')<CR>",
      mode = { "n", "o", "x" },
      desc = "Move to start of previous word",
    },
  },
}
