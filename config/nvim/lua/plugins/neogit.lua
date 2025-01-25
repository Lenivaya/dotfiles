return {
  "NeogitOrg/neogit",
  dependencies = {
    "nvim-lua/plenary.nvim", -- required
    "sindrets/diffview.nvim", -- optional - Diff integration

    -- Only one of these is needed.
    "nvim-telescope/telescope.nvim", -- optional
    -- "ibhagwan/fzf-lua", -- optional
    -- "echasnovski/mini.pick", -- optional
  },
  config = function(_, opts)
    local neogit = require("neogit")

    vim.keymap.set("", "<leader>gn", neogit.open, { remap = true, desc = "Open neogit" })

    neogit.setup(opts)
  end,
  -- config = true,
}
