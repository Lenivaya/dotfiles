local A = vim.api

return {
  "s1n7ax/nvim-window-picker",
  name = "window-picker",
  event = "VeryLazy",
  lazy = true,
  version = "2.*",
  opts = {
    hint = "floating-letter",
    filter_rules = {
      include_current_win = false,
      autoselect_one = true,
      bo = {
        filetype = { "neo-tree" },
      },
    },
  },
  keys = function()
    local function pick_window()
      local picked_window = require("window-picker").pick_window()
      if picked_window then
        A.nvim_set_current_win(picked_window)
      end
    end
    local function kill_window()
      local picked = require("window-picker").pick_window({
        include_current_win = true,
      })
      if not picked then
        return
      end
      A.nvim_win_close(picked, false)
    end

    return {
      {
        "<leader>w/",
        pick_window,
        desc = "Pick window to switch to",
        remap = true,
      },
      {
        "<c-w>/",
        pick_window,
        desc = "Pick window to switch to",
        remap = true,
      },
      {
        "<leader>wX",
        kill_window,
        desc = "Pick window to kill",
        remap = true,
      },
      {
        "<c-w>X",
        kill_window,
        desc = "Pick window to kill",
        remap = true,
      },
    }
  end,
}
