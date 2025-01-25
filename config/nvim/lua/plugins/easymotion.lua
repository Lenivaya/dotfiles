return {
  "smoka7/hop.nvim",
  version = "*",
  lazy = false,
  opts = {
    keys = "etovxqpdygfblzhckisuran",
  },
  cmd = { "HopWord", "HopLine", "HopLineStart", "HopWordCurrentLine" },
  config = function(_, opts)
    -- dofile(vim.g.base46_cache .. "hop")
    -- require("hop").setup(opts)

    local hop = require("hop")
    local directions = require("hop.hint").HintDirection
    -- vim.keymap.set("", "f", function()
    --   hop.hint_char1({ direction = directions.AFTER_CURSOR, current_line_only = true })
    -- end, { remap = true })
    -- vim.keymap.set("", "F", function()
    --   hop.hint_char1({ direction = directions.BEFORE_CURSOR, current_line_only = true })
    -- end, { remap = true })
    -- vim.keymap.set("", "t", function()
    --   hop.hint_char1({ direction = directions.AFTER_CURSOR, current_line_only = true, hint_offset = -1 })
    -- end, { remap = true })
    -- vim.keymap.set("", "T", function()
    --   hop.hint_char1({ direction = directions.BEFORE_CURSOR, current_line_only = true, hint_offset = 1 })
    -- end, { remap = true })

    vim.keymap.set("", "<leader>jw", hop.hint_words, { remap = true, desc = "Hop to a word" })
    vim.keymap.set("", "<leader>j<leader>", hop.hint_patterns, { remap = true, desc = "Hop to a pattern" })
    vim.keymap.set("", "<leader>jl", hop.hint_lines_skip_whitespace, { remap = true, desc = "Hop to a line" })

    hop.setup(opts)
  end,
}
