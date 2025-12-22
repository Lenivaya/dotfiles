return {
  {
    "nvim-lualine/lualine.nvim",
    opts = function(_, opts)
      -- Disable time
      opts.sections.lualine_z = {}

      opts.sections.lualine_c[4] = { LazyVim.lualine.pretty_path({
        length = 6,
      }) }

      -- Show lualine on top, much more useful for me
      opts.tabline = opts.sections
      opts.sections = {}
    end,
  },
}
