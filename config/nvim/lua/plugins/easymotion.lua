return {
  {
    "folke/flash.nvim",
    keys = {
      {
        "<C-;>",
        mode = { "n", "x", "o" },
        function()
          require("flash").jump({
            pattern = ".", -- initialize pattern with any char
            search = {
              mode = function(pattern)
                -- remove leading dot
                if pattern:sub(1, 1) == "." then
                  pattern = pattern:sub(2)
                end
                -- return word pattern and proper skip pattern
                return ([[\<%s\w*\>]]):format(pattern), ([[\<%s]]):format(pattern)
              end,
            },
            -- select the range
            jump = { pos = "range" },
          })
        end,
        desc = "Jump on sole occurence",
        remap = true,
      },
    },
  },
}
