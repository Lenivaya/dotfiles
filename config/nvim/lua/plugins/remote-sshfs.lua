return {
  "nosduco/remote-sshfs.nvim",
  dependencies = { "nvim-telescope/telescope.nvim" },
  opts = {
    connections = {
      ssh_configs = { -- which ssh configs to parse for hosts list
        vim.fn.expand("$HOME") .. "/.ssh/config",
        "/etc/ssh/ssh_config",
        -- "/path/to/custom/ssh_config"
      },
    },
    -- Refer to the configuration section below
    -- or leave empty for defaults
  },
}
