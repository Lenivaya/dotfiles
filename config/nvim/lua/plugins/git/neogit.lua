return {
  "NeogitOrg/neogit",
  event = "VeryLazy",
  dependencies = {
    "nvim-lua/plenary.nvim", -- required
    "sindrets/diffview.nvim", -- optional - Diff integration
    -- Only one of these is needed.
    "nvim-telescope/telescope.nvim", -- optional
    -- "ibhagwan/fzf-lua", -- optional
    -- "echasnovski/mini.pick", -- optional
  },
  cmd = "Neogit",
  keys = {
    { "<leader>gn", "<cmd>Neogit<cr>", desc = "Neogit" },
    {
      "<leader>gC",
      function()
        require("neogit").open({ "commit" })
      end,
      desc = "Neogit Open",
    },
  },
  config = true,
}
