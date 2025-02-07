return {
  "tris203/precognition.nvim",
  lazy = true,
  opts = {
    startVisible = false,
  },
  keys = {
    {
      "<leader>u?",
      function()
        require("precognition").toggle()
      end,
      desc = "Toggle Precognition",
    },
  },
}
