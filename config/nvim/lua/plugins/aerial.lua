return {
  "stevearc/aerial.nvim",
  event = "LazyFile",
  opts = function(_, opts)
    table.insert(opts.layout, {
      resize_to_content = true,
      -- max_width = {40, 0.2} means "the lesser of 40 columns or 20% of total"
      max_width = 0.55,
      min_width = 0.15,
    })
  end,
}
