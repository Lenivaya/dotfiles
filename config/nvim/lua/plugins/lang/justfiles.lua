return {
  {
    "chrisgrieser/nvim-justice",
    keys = {
      {
        "<leader>j",
        function()
          require("justice").select()
        end,
        desc = "󰖷 Just",
      },
    },
    opts = {
      recipes = {
        ignore = {
          name = {},
          comment = {},
        },
        streaming = {
          name = { "download" },
          comment = { "streaming", "curl" },
        },
        quickfix = {
          name = { "%-qf$" },
          comment = { "quickfix" },
        },
        terminal = {
          name = { "release" },
          comment = { "in the terminal" },
        },
      },
      window = { border = vim.g.borderStyle },
      keymaps = {
        closeWin = { "q", "<Esc>", "<D-w>" },
        quickSelect = { "j", "f", "d", "s", "a" },
      },
    },
  },
  -- {
  --   "al1-ce/just.nvim",
  --   event = "VeryLazy",
  --   ft = { "just" },
  --   dependencies = {
  --     "nvim-lua/plenary.nvim", -- async jobs
  --     "nvim-telescope/telescope.nvim", -- task picker
  --     "rcarriga/nvim-notify", -- general notifications (optional)
  --     "j-hui/fidget.nvim", -- task progress
  --     "al1-ce/jsfunc.nvim", -- extension library
  --   },
  --   config = true,
  -- },
}
