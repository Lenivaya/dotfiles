vim.api.nvim_create_autocmd("VimEnter", {
  callback = function()
    vim.cmd(":silent !kitty @ set-spacing padding=0 margin=0")
  end,
})

vim.api.nvim_create_autocmd("VimLeavePre", {
  callback = function()
    vim.cmd(":silent !kitty @ set-spacing padding=20 margin=10")
  end,
})
