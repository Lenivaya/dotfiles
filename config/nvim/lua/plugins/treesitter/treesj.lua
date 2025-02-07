return {
  "Wansmer/treesj",
  vscode = true,
  keys = {
    {
      "J",
      "<cmd>TSJToggle<cr>",
      desc = "Join Toggle",
      remap = true,
    },
  },
  opts = {
    use_default_keymaps = false,
    max_join_length = 150,
  },
}
