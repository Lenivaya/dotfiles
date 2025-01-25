vim.api.nvim_create_autocmd("VimEnter", {
  callback = function()
    if vim.env.KITTY_WINDOW_ID then
      vim.cmd(":silent !kitty @ set-spacing padding=0 margin=0")
    end
  end,
})

vim.api.nvim_create_autocmd("VimLeavePre", {
  callback = function()
    if vim.env.KITTY_WINDOW_ID then
      vim.cmd(":silent !kitty @ set-spacing padding=20 margin=10")
    end
  end,
})
