-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

local set = vim.keymap.set

set({ "i", "v", "n", "s" }, "<leader>fs", "<cmd>w<cr><esc>", { desc = "Save file" })
set("n", "<leader>;", "<cmd>e#<cr>", { desc = "Last Buffer" })
set("n", "<leader>bn", "<cmd>enew<cr>", { desc = "New buffer" })
set("n", "<C-a>", "ggVG", { desc = "Select all" })
