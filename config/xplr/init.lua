version = "0.19.0"

local xplr = xplr

xplr.config.general.enable_mouse = true
xplr.config.general.show_hidden = true
xplr.config.general.enable_recover_mode = false

-- KEYS: default mode {{{
local key = xplr.config.modes.builtin.default.key_bindings.on_key

key.e = xplr.config.modes.builtin.action.key_bindings.on_key.e -- open editor
key.o = xplr.config.modes.builtin.go_to.key_bindings.on_key.x -- open gui

-- shell
key['!'] = {
  help = "shell",
  messages = {
    { Call = { command = "zsh", args = { "-i", "-l" } } }, "ExplorePwdAsync",
    "PopMode"
  }
}

-- Usage: dua i
key.D = {
  help = "disk usage",
  messages = { { BashExec = [[dua i]] }, "ClearScreen" }
}

-- Preview: fzf
key["ctrl-f"] = {
  help = "search with preview",
  messages = {
    {
      BashExec = [===[
      PTH=$(cat "${XPLR_PIPE_DIRECTORY_NODES_OUT:?}" \
        | awk -F / '{print $NF}' \
        | fzf --preview-window ":nohidden")
      if [ -d "$PTH" ]; then
        echo ChangeDirectory: "'"$PWD/${PTH:?}"'" >> "${XPLR_PIPE_MSG_IN:?}"
      elif [ -f "$PTH" ]; then
        echo FocusPath: "'"$PWD/${PTH:?}"'" >> "${XPLR_PIPE_MSG_IN:?}"
      fi
      ]===]
    }
  }
}

-- Fuzzy search history
xplr.config.modes.builtin.go_to.key_bindings.on_key.h = {
  help = "history",
  messages = {
    "PopMode",
    {
      BashExec = [===[
        PTH=$(cat "${XPLR_PIPE_HISTORY_OUT:?}" | sort -u | fzf --no-sort)
        if [ "$PTH" ]; then
          echo ChangeDirectory: "'"${PTH:?}"'" >> "${XPLR_PIPE_MSG_IN:?}"
        fi
      ]===],
    },
  },
}

-- Batch rename
key.R = {
  help = "batch rename",
  messages = {
    {
      BashExec = [[
      SELECTION=$(cat "${XPLR_PIPE_SELECTION_OUT:?}")
      NODES=${SELECTION:-$(cat "${XPLR_PIPE_DIRECTORY_NODES_OUT:?}")}
      if [ "$NODES" ]; then
        echo -e "$NODES" | pipe-renamer
        echo ExplorePwdAsync >> "${XPLR_PIPE_MSG_IN:?}"
      fi
    ]]
    }
  }
}

-- Image viewer (imv)
key.P = {
  help = "preview",
  messages = {
    {
      BashExecSilently = [===[
        FIFO_PATH="/tmp/xplr.fifo"

        if [ -e "$FIFO_PATH" ]; then
          echo StopFifo >> "$XPLR_PIPE_MSG_IN"
          rm "$FIFO_PATH"
        else
          mkfifo "$FIFO_PATH"
          "imv-open" "$FIFO_PATH" "$XPLR_FOCUS_PATH" &
          echo "StartFifo: '$FIFO_PATH'" >> "$XPLR_PIPE_MSG_IN"
        fi
      ]===],
    },
  },
}

-- Zoxide (better cd in rust)
key["Z"] = {
    help = "zoxide jump",
    messages = {
      {
        BashExec = [===[
        PTH=$(zoxide query -i)
        if [ -d "$PTH" ]; then
          echo ChangeDirectory: "'"${PTH:?}"'" >> "${XPLR_PIPE_MSG_IN:?}"
        fi
        ]===]
      },
      "PopMode",
    }
  }
