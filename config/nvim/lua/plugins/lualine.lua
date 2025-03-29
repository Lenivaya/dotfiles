return {
  {
    "nvim-lualine/lualine.nvim",
    opts = function(_, opts)
      -- Disable time
      opts.sections.lualine_z = {}

      -- Show lualine on top, much more useful for me
      opts.tabline = opts.sections
      opts.sections = {}
    end,
  },
}
