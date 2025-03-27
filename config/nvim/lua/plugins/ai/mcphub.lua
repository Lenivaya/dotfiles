return {
  "ravitemer/mcphub.nvim",
  dependencies = {
    "nvim-lua/plenary.nvim", -- Required for Job and HTTP requests
  },
  -- cmd = "MCPHub", -- lazily start the hub when `MCPHub` is called
  build = "pnpm install -g mcp-hub@latest", -- Installs required mcp-hub npm module
  config = function()
    require("mcphub").setup({
      -- Required options
      port = 6666, -- Port for MCP Hub server
      config = vim.fn.expand("~/mcpservers.json"), -- Absolute path to config file

      -- Optional options
      on_ready = function(hub)
        -- Called when hub is ready
      end,
      on_error = function(err)
        -- Called on errors
      end,
      log = {
        level = vim.log.levels.WARN,
        to_file = false,
        file_path = nil,
        prefix = "MCPHub",
      },
    })
  end,
}
