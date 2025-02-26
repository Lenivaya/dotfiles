return {
  {
    "axieax/urlview.nvim",
    cmd = "UrlView",

    config = function(_, opts)
      require("urlview").setup(opts)
    end,

    opts = {
      -- custom configuration options --
    },

    keys = {
      { "<leader>fuu", "<Cmd>UrlView<CR>", mode = "n", desc = "View buffer URLs" },
      { "<leader>ful", "<Cmd>UrlView lazy<CR>", mode = "n", desc = "View Lazy URLs" },
      { "<leader>fup", "<Cmd>UrlView packer<CR>", mode = "n", desc = "View Packer plugin URLs" },
      { "<leader>fuv", "<Cmd>UrlView vimplug<CR>", mode = "n", desc = "View Packer plugin URLs" },
    },
  },
}
