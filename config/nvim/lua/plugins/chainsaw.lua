return {
  "chrisgrieser/nvim-chainsaw",
  lazy = true,
  config = true,
  evenet = "VeryLazy",
  keys = {
    {
      "<leader>ddv",
      function()
        require("chainsaw").variableLog()
      end,
      desc = "Add Variable Log",
    },
    {
      "<leader>ddo",
      function()
        require("chainsaw").objectLog()
      end,
      desc = "Add Object Log",
    },
    {
      "<leader>ddi",
      function()
        require("chainsaw").objectLog()
      end,
      desc = "Add Object Log",
    },
    {
      "<leader>dda",
      function()
        require("chainsaw").assertLog()
      end,
      desc = "Add Assert Log",
    },
    {
      "<leader>ddm",
      function()
        require("chainsaw").messageLog()
      end,
      desc = "Add Message Log",
    },
    {
      "<leader>dds",
      function()
        require("chainsaw").stracktraceLog()
      end,
      desc = "Add Stacktrace",
    },
    {
      "<leader>ddb",
      function()
        require("chainsaw").beepLog()
      end,
      desc = "Add Beep Log",
    },
    {
      "<leader>ddx",
      function()
        require("chainsaw").removeLogs()
      end,
      desc = "Remove Log Statements",
    },
  },
}
