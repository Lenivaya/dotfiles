return {
  "CopilotC-Nvim/CopilotChat.nvim",
  event = "VeryLazy",
  keys = {
    { "<leader>a", false },
    {
      "<leader>aa",
      false,
    },
    {
      "<leader>ax",
      false,
    },
    {
      "<leader>aq",
      false,
    },

    { "<leader>ac", "", desc = "+copilot", mode = { "n", "v" } },
    {
      "<leader>aca",
      function()
        return require("CopilotChat").toggle()
      end,
      desc = "Toggle (CopilotChat)",
      mode = { "n", "v" },
    },
    {
      "<leader>acx",
      function()
        return require("CopilotChat").reset()
      end,
      desc = "Clear (CopilotChat)",
      mode = { "n", "v" },
    },
    {
      "<leader>acq",
      function()
        local input = vim.fn.input("Quick Chat: ")
        if input ~= "" then
          require("CopilotChat").ask(input)
        end
      end,
      desc = "Quick Chat (CopilotChat)",
      mode = { "n", "v" },
    },
  },
}
