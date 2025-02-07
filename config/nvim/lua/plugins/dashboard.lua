return {
  {
    "folke/snacks.nvim",
    opts = function(_, opts)
      local logo = ""
      opts.dashboard.preset.header = logo
    end,
  },
}
