# file-extra-metadata

<!--toc:start-->

- [file-extra-metadata](#file-extra-metadata)
  - [Preview](#preview)
    - [Before:](#before)
    - [After:](#after)
  - [Requirements](#requirements)
  - [Installation](#installation)
  - [For developer](#for-developer)
  <!--toc:end-->

This is a Yazi plugin that replaces the default file previewer and spotter with extra information.

> [!IMPORTANT]
> Minimum version: yazi v25.2.7

## Preview

### Before:

- Previewer

  ![Before preview](statics/2024-11-17-12-06-24.png)

- Spotter

  ![Before spot](statics/2024-11-21-04-19-01.png)

### After:

- Previewer

![After previewer](statics/2024-11-21-05-27-48.png)

- Spotter

![After spotter](statics/2024-11-21-05-29-50.png)

## Requirements

- [yazi >= 25.2.7](https://github.com/sxyazi/yazi)
- Tested on Linux. For MacOS, Windows: some fields will shows empty values.

## Installation

Install the plugin:

```sh
ya pack -a boydaihungst/file-extra-metadata
```

Add spotter keybind, makes sure not conflict with other `<Tab>` keybind in
`manager` section:

```toml
[manager]
keymap = [
  # ...
  # Spotting
  { on = "<Tab>", run = "spot", desc = "Spot hovered file" },
]
```

Create `~/.config/yazi/yazi.toml` and add:

```toml
[plugin]
  append_previewers = [
    { name = "*", run = "file-extra-metadata" },
  ]
  # yazi v0.4 after 21/11/2024
  # Setup keybind for spotter: https://github.com/sxyazi/yazi/pull/1802
  append_spotters = [
    { name = "*", run = "file-extra-metadata" },
  ]
```

or

```toml
[plugin]
  previewers = [
    # ... the rest
    # disable default file plugin { name = "*", run = "file" },
    { name = "*", run = "file-extra-metadata" },
  ]
  # yazi v0.4 after 21/11/2024
  # Setup keybind for spotter: https://github.com/sxyazi/yazi/pull/1802
  spotters = [
    # ... the rest
    # Fallback
    # { name = "*", run = "file" },
    { name = "*", run = "file-extra-metadata" },
  ]
```

### Custom theme

Read more: https://github.com/sxyazi/yazi/pull/2391

Edit or add `yazi/theme.toml`:

```toml
[spot]
border = { fg = "#4fa6ed" }
title = { fg = "#4fa6ed" }

# Table. Currently only support nightly.
tbl_cell = { fg = "#4fa6ed", reversed = true }
tbl_col = { fg = "#4fa6ed" }
```

## For developer

If you want to compile this with other spotter/previewer:

```lua
require("file-extra-metadata"):render_table(job, { show_plugins_section = true })
```
