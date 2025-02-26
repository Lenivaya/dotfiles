return {
  -- Hide Secrets
  {
    "laytan/cloak.nvim",
    event = "VeryLazy",
    version = false,
    opts = function(_, opts)
      opts.cloak_length = 5
      opts.cloak_on_leave = true

      require("which-key").add({
        { "<leader>*", group = "Cloak", icon = LazyVim.config.icons.misc.dots },
        { "<leader>*l", "<cmd>CloakPreviewLine<cr>", desc = "Uncloak Line" },
      })

      Snacks.toggle({
        name = "Cloak",
        get = function()
          return vim.b.cloak_enabled ~= false
        end,
        set = function()
          require("cloak").toggle()
        end,
      }):map("<leader>**")
    end,
  },
}
