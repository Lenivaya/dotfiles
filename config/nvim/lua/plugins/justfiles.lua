return {
  {
    "al1-ce/just.nvim",
    event = "VeryLazy",
    ft = { "just" },
    dependencies = {
      "nvim-lua/plenary.nvim", -- async jobs
      "nvim-telescope/telescope.nvim", -- task picker
      "rcarriga/nvim-notify", -- general notifications (optional)
      "j-hui/fidget.nvim", -- task progress
      "al1-ce/jsfunc.nvim", -- extension library
    },
    config = true,
  },
}
