return {
  "CopilotC-Nvim/CopilotChat.nvim",
  event = "VeryLazy",
  options = {
    model = "claude-3.5-sonnet",
  },
  keys = {
    { "<leader>a", false },
    {
      "<leader>aa",
      false,
      mode = { "n", "v" },
    },
    {
      "<leader>ax",
      false,
      mode = { "n", "v" },
    },
    {
      "<leader>aq",
      false,
      mode = { "n", "v" },
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
