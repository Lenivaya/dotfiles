-- https://github.com/minusfive/dotfiles/blob/1bd0420d662e319ced6dcd791d0cd77feb60e01e/.config/nvim/lua/plugins/obsidian.lua

local vault_path = "~/Knowledge-base"
local vault_path_absolute = vim.fn.expand(vault_path)

return {
  "epwalsh/obsidian.nvim",
  version = "*", -- recommended, use latest release instead of latest commit
  lazy = true,
  -- ft = "markdown",
  -- Replace the above line with this if you only want to load obsidian.nvim for markdown files in your vault:
  -- event = {
  --   -- If you want to use the home shortcut '~' here you need to call 'vim.fn.expand'.
  --   -- E.g. "BufReadPre " .. vim.fn.expand "~" .. "/my-vault/*.md"
  --   -- refer to `:h file-pattern` for more examples
  --   "BufReadPre path/to/my-vault/*.md",
  --   "BufNewFile path/to/my-vault/*.md",
  -- },
  event = {
    -- if you want to use the home shortcut '~' here you need to call 'vim.fn.expand'.
    -- e.g. "bufreadpre " .. vim.fn.expand "~" .. "/my-vault/**.md"
    "bufreadpre "
      .. vault_path_absolute
      .. "/**.md",
    "bufnewfile " .. vault_path_absolute .. "/**.md",
  },
  dependencies = {
    -- Required.
    "nvim-lua/plenary.nvim",
    -- see below for full list of optional dependencies 👇
  },
  keys = {
    { "<leader>fN", "<cmd>ObsidianQuickSwitch<cr>", desc = "New Note (Obsidian)", remap = true },
    { "<leader>fod", "<cmd>ObsidianToday<cr>", desc = "Daily note (Obsidian)", remap = true },
    { "<leader>foD", "<cmd>ObsidianDailies<cr>", desc = "Daily notes picker (Obsidian)", remap = true },
    { "<leader>fob", "<cmd>ObsidianiBacklinks<cr>", desc = "Note backlinks (Obsidian)", remap = true },
    { "<leader>fol", "<cmd>ObsidianLinks<cr>", desc = "Link note (Obsidian)", remap = true },
    { "<leader>foL", "<cmd>ObsidianLink<cr>", desc = "Link to note (Obsidian)", remap = true },
    { "<leader>fot", "<cmd>ObsidianTags<cr>", desc = "Note tags search (Obsidian)", remap = true },
    { "<leader>foi", "<cmd>ObsidianPasteImg<cr>", desc = "Paste image from clipboard (Obsidian)", remap = true },
    { "<leader>for", "<cmd>ObsidianRename<cr>", desc = "Rename note (Obsidian)", remap = true },
    { "<leader>fot", "<cmd>ObsidianTOC<cr>", desc = "Table of contents (Obsidian)", remap = true },
    { "<leader>foe", "<cmd>ObsidianExtractNote<cr>", desc = "Extract note (Obsidian)", remap = true },
  },
  opts = {
    workspaces = {
      {
        name = "personal",
        path = "~/Sync/Knowledge-base",
      },
    },
    -- see below for full list of options 👇
    notes_subdir = "notes",
    new_notes_location = "notes_subdir",

    sort_by = "modified",
    sort_reversed = true,

    attachments = {
      -- The default folder to place images in via `:ObsidianPasteImg`.
      -- If this is a relative path it will be interpreted as relative to the vault root.
      -- You can always override this per image by passing a full path to the command instead of just a filename.
      img_folder = "assets/imgs", -- This is the default
    },

    daily_notes = {
      -- Optional, if you keep daily notes in a separate directory.
      folder = "notes/dailies",
      -- Optional, if you want to change the date format for the ID of daily notes.
      date_format = "%Y-%m-%d",
      -- Optional, if you want to change the date format of the default alias of daily notes.
      alias_format = "%B %-d, %Y",
      -- Optional, default tags to add to each new daily note created.
      default_tags = { "daily-notes" },
      -- Optional, if you want to automatically insert a template from your template directory like 'daily.md'
      template = nil,
    },
  },
}
