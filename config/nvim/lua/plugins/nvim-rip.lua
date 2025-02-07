return {
  "chrisgrieser/nvim-rip-substitute",
  keys = {
    {
      "g/",
      function()
        require("rip-substitute").sub()
      end,
      mode = { "n", "x" },
      desc = "Rip Substitute",
    },
    {
      "<leader>rs",
      function()
        require("rip-substitute").sub()
      end,
      mode = { "n", "x" },
      desc = " rip-substitute",
    },
    {
      "<leader>rS",
      function()
        require("rip-substitute").rememberCursorWord()
      end,
      desc = " remember cword (rip-sub)",
    },
  },
  opts = {
    popupWin = {
      hideKeymapHints = false,
    },
    editingBehavior = {
      autoCaptureGroups = true,
    },
  },
}
