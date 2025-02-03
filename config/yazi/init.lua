require("full-border"):setup()
require("starship"):setup()
require("git"):setup()
require("relative-motions"):setup({ show_numbers = "relative", show_motion = true, enter_mode = "first" })
require("projects"):setup({
  save = {
    method = "yazi", -- yazi | lua
  },
  last = {
    update_after_save = true,
    update_after_load = true,
    load_after_start = false,
  },
  merge = {
    quit_after_merge = false,
  },
  notify = {
    enable = true,
    title = "Projects",
    timeout = 3,
    level = "info",
  },
})

require("yatline"):setup({
  show_background = false,

  header_line = {
    left = {
      section_a = {
        { type = "line", custom = false, name = "tabs", params = { "left" } },
      },
      section_b = {},
      section_c = {},
    },
    right = {
      section_a = {
        { type = "string", custom = false, name = "date", params = { "%A, %d %B %Y" } },
      },
      section_b = {
        { type = "string", custom = false, name = "date", params = { "%X" } },
      },
      section_c = {},
    },
  },

  status_line = {
    left = {
      section_a = {
        { type = "string", custom = false, name = "tab_mode" },
      },
      section_b = {
        { type = "string", custom = false, name = "hovered_size" },
      },
      section_c = {
        { type = "string", custom = false, name = "hovered_path" },
        { type = "coloreds", custom = false, name = "count" },
      },
    },
    right = {
      section_a = {
        { type = "string", custom = false, name = "cursor_position" },
      },
      section_b = {
        { type = "string", custom = false, name = "cursor_percentage" },
      },
      section_c = {
        { type = "string", custom = false, name = "hovered_file_extension", params = { true } },
        { type = "coloreds", custom = false, name = "permissions" },
      },
    },
  },
})
