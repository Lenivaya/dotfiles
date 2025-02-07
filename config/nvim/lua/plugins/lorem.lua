return {
  event = "VeryLazy",
  "elvxk/mylorem.nvim",
  lazy = true,
  config = function()
    require("mylorem").setup({
      luasnip = true, -- Enable for LuaSnip
      default = true, -- Use LuaSnip by default
    })
  end,
}
