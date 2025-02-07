return {
  "HakonHarnes/img-clip.nvim",
  event = "VeryLazy",
  opts = {
    -- add options here
    -- or leave it empty to use the default settings
    use_absolute_path = false,
    relative_to_current_file = true, ---@type boolean | fun(): boolean
  },
  keys = {
    -- suggested keymap
    { "<leader>aI", "<cmd>PasteImage<cr>", desc = "Paste image from system clipboard" },
  },
}
