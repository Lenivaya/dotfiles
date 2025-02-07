return {
  "ysmb-wtsg/in-and-out.nvim",
  evnt = "VeryLazy",
  keys = {
    {
      "<C-CR>",
      function()
        require("in-and-out").in_and_out()
      end,
      mode = { "i" },
      desc = "In and Out",
    },
  },
}
