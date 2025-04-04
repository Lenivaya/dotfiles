-- Define static theme configurations
local catppuccin_config = {
  -- https://github.com/catppuccin/nvim#integrations
  integrations = {
    hop = true,
    mason = true,
    grug_far = true,
    cmp = true,
    blink_cmp = true,
    noice = true,
    gitsigns = true,
    treesitter_context = true,
    copilot_vim = true,
    ufo = true,
    octo = true,
    window_picker = true,
    neotest = true,
    treesitter = true,
    gitgraph = true,
    flash = true,
    avante = true,
    snacks = true,
    gitgutter = true,
  },
  default_integrations = true,
  no_italic = false,
  no_bold = false,
  no_underline = false,
  term_colors = false,
  show_end_of_buffer = false,
  transparent_background = true,
  flavor = "mocha",
  color_overrides = {
    mocha = {
      base = "#000000",
      mantle = "#000000",
      crust = "#000000",
    },
  },
  styles = {
    comments = { "italic" },
    keywords = { "italic" },
    conditionals = { "italic" },
  },
}

local solarized_config = {
  variant = "summer",
  transparent = {
    enabled = true, -- Master switch to enable transparency
    pmenu = true, -- Popup menu (e.g., autocomplete suggestions)
    normal = true, -- Main editor window background
    normalfloat = true, -- Floating windows
    neotree = true, -- Neo-tree file explorer
    nvimtree = true, -- Nvim-tree file explorer
    whichkey = true, -- Which-key popup
    telescope = true, -- Telescope fuzzy finder
    lazy = true, -- Lazy plugin manager UI
    mason = true, -- Mason manage external tooling
  },
}

-- Helper function to get the system theme from darkman
local function get_system_theme()
  local handle = io.popen("darkman get")
  if not handle then
    return "dark" -- default fallback
  end
  local result = handle:read("*a")
  handle:close()
  return result:gsub("^%s+", ""):gsub("%s+$", "")
end

-- Define our reload_theme function
local function reload_theme()
  if vim.g.vscode then
    return -- Skip if running in VSCode
  end

  local bg = vim.o.background

  if bg == "dark" then
    -- Load catppuccin with dark mode settings
    require("catppuccin").setup(catppuccin_config)
    vim.cmd.colorscheme("catppuccin")
  else
    -- Load solarized with light mode settings
    require("solarized").setup(solarized_config)
    vim.cmd.colorscheme("solarized")
  end

  -- Additional customizations that apply after theme load
  -- vim.cmd.hi 'MatchParen guifg=NONE guibg=#CCCCCC gui=bold,underline'
end

-- Function to apply system theme
local function apply_system_theme()
  if vim.g.vscode then
    return -- Skip if running in VSCode
  end

  -- Get system theme from darkman
  local success, mode = pcall(get_system_theme)
  if success then
    if mode == "dark" then
      vim.o.background = "dark"
    elseif mode == "light" then
      vim.o.background = "light"
    end

    -- Apply the theme based on system setting
    reload_theme()
  end
end

-- Create augroup for our autocmds
local augroup = vim.api.nvim_create_augroup("ColorSchemeEvents", {
  clear = true,
})

-- Set the theme on VimEnter
vim.api.nvim_create_autocmd("VimEnter", {
  pattern = "*",
  group = augroup,
  callback = function()
    if vim.g.vscode then
      return -- Skip if running in VSCode
    end

    -- Defer to ensure it runs after initialization
    vim.defer_fn(function()
      apply_system_theme()
    end, 1) -- Short delay
  end,
  once = true,
})

-- Set up autocmd to reload theme when background option changes
vim.api.nvim_create_autocmd("OptionSet", {
  pattern = "background",
  group = augroup,
  callback = reload_theme,
})

return {
  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = "catppuccin", -- Let LazyVim initialize with its default theme
    },
  },
  {
    -- Main dark theme
    "catppuccin/nvim",
    name = "catppuccin",
    lazy = false,
    priority = 1000,
    version = false, -- always use the latest git commit
    vscode = false,
    opts = catppuccin_config, -- Pass the static config directly as opts
  },
  {
    -- Main light theme
    "maxmx03/solarized.nvim",
    lazy = false,
    priority = 1000,
    opts = solarized_config, -- Pass the static config directly as opts
  },
  {
    -- Darkman integration
    "4e554c4c/darkman.nvim",
    lazy = false,
    build = "go build -o bin/darkman.nvim",
    priority = 1000,
    opts = {
      change_background = true,
      send_user_event = true,
    },
  },
}
