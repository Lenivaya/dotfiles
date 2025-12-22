# auto-layout.yazi

> [!WARNING]
> This plugin is longer compatible with yazi starting with version 25.x. Unfortunately this is a breaking change with the Tab layout functions this plugin depended on, and it seems like there's no solution using the modern syntax. If `yazi` ever adds official support for listening to layout changes, this plugin will be updated to support it. Until then, if you're on the latest `yazi` version you should not use this plugin.

This plugin for the [yazi file explorer](https://yazi-rs.github.io) will automatically change the number of columns to show in yazi based on the available width. This is especially useful if you use a terminal layout that might have yazi run in a sidebar (where 1 column is all that's required) but then sometimes zoom into it and you want it to update to use the full 3 column layout.

## Installation

```sh
$ ya pack -a josephschmitt/auto-layout
```

## Usage

```lua
-- In your yazi config's init.lua
require("auto-layout")
```

If you want to customize the breakpoints where the column shifts happen:

```lua
require("auto-layout").setup({
   breakpoint_large = 110,  -- new large window threshold, defaults to 100
   breakpoint_medium = 60,  -- new medium window threshold, defaults to 50
 })
```
