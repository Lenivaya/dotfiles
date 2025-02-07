return {
  "nativerv/cyrillic.nvim",
  vscode = true,
  event = { "VeryLazy" },
  config = function()
    require("cyrillic").setup({
      no_cyrillic_abbrev = false, -- default
    })
  end,
}
