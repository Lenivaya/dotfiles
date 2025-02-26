return {
  "aaronik/treewalker.nvim",
  vscode = true,
  opts = {},
  keys = {
    {
      "<S-Tab>",
      function()
        require("treewalker").move_up()
      end,
      desc = "Treewalker Up",
    },
    {
      "<Tab>",
      function()
        require("treewalker").move_down()
      end,
      desc = "Treewalker Down",
    },
    {
      "}",
      function()
        require("treewalker").move_in()
      end,
      desc = "Treewalker Right",
    },
    {
      "{",
      function()
        require("treewalker").move_out()
      end,
      desc = "Treewalker Left",
    },
    {
      "J",
      function()
        require("treewalker").swap_down()
      end,
      desc = "Treewalker Swap Down",
    },
    {
      "K",
      function()
        require("treewalker").swap_up()
      end,
      desc = "Treewalker Swap Up",
    },
  },
}
