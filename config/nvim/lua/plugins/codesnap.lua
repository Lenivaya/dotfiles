return {
  "mistricky/codesnap.nvim",
  build = "make",
  event = "VeryLazy",
  -- keys = {
  --   { "<leader>cc", "<cmd>CodeSnap<cr>", mode = "x", desc = "Save selected code snapshot into clipboard" },
  --   { "<leader>cs", "<cmd>CodeSnapSave<cr>", mode = "x", desc = "Save selected code snapshot in ~/Pictures" },
  -- },
  opts = {
    save_path = "~/Pictures/Screenshots/",
    has_breadcrumbs = false,
    bg_color = "#535c68",
    -- bg_padding = 0,
    bg_x_padding = 100,
    bg_y_padding = 40,
    watermark = "",
    mac_window_bar = false,
    -- code_font_family = "monospace",
    code_font_family = "PragmataPro Mono Liga Regular",
  },
}
