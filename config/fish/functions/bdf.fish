# The MIT License (MIT)
#
# Copyright (c) 2023 Ryotaro Jusin Kimura
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.



function bdf
    test -z "$BDF_SELECTOR" && set -x BDF_SELECTOR fzf
    test -z "$BDF_LS" && set -x BDF_LS "lsd -A"

    set -l current_directory (pwd)
    set -l parent_directories
    set -l selected_directory

    set current_parts (string split / $current_directory)

    for i in (seq (count $current_parts) -1 2)
        set parent_directories[$i] (string join / $current_parts[1..$i])
    end

    set parent_directories[1] /
    set parent_directories $parent_directories[-1..1]
    set -e parent_directories[1]

    set -l selected_path (echo (string join ' ' $parent_directories) |  tr ' ' '\n' | $BDF_SELECTOR --preview "$BDF_LS {}" --select-1)

    test -n "$selected_path" && cd $selected_path
end
