-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here

local opt = vim.opt

opt.encoding = "utf-8"
opt.fileencoding = "utf-8"

opt.number = true
opt.spell = false
-- opt.conceallevel = 2

vim.opt.title = true
vim.opt.autoindent = true
vim.opt.smartindent = true
vim.opt.termguicolors = true -- Enable 24-bit RGB colors vim.opt.laststatus = 2 -- Set global statusline
vim.opt.ignorecase = true -- Ignore case letters when search

opt.hlsearch = true -- highlight search
opt.incsearch = true -- incremental search
opt.smartcase = true -- if you include mixed case in your search, assumes you want case-sensitive
opt.grepprg = "rg --vimgrep --no-heading --smart-case" -- use ripgrep for grep

vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.writebackup = false
vim.opt.showcmd = true
vim.opt.backupskip = { "/tmp/*", "/private/tmp/*" }
vim.opt.path:append({ "**" }) -- Finding files - Search down into subfolders
vim.opt.wildignore:append({ "*/node_modules/*" })
vim.g.deprecation_warnings = false

vim.g.lazyvim_picker = "snacks"
vim.g.lazyvim_explorer = "snacks"
vim.g.lazyvim_blink_main = false

-- Don't need animations
-- vim.g.snacks_animate = false
