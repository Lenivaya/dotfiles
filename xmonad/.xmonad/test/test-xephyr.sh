#!/bin/sh -eu

usage () {
    cat << EOF
Usage: test-xephyr.sh [options]

    -d NxN  Set the screen size to NxN
    -h      This message
    -n NUM  Set the internal DISPLAY to NUM
    -s NUM  Set the number of screens to NUM
EOF
}

ARCH=$(uname -m)
OS=$(uname -s | tr '[:upper:]' '[:lower:]')
SCREENS=1
SCREEN_SIZE="800x600"
DISPLAY_NUMBER=9

while getopts "d:hs:n:" OPT
do
    case "${OPT}" in
        d)
            SCREEN_SIZE=$OPTARG
        ;;

        h)
            usage && exit
        ;;

        n)
            DISPLAY_NUMBER=$OPTARG
        ;;

        s)
            SCREENS=$OPTARG
        ;;

        *)
            usage && exit 1
        ;;
    esac
done

shift $((OPTIND-1))

if [ ! "$(command -v Xephyr)" ]
then
    echo "test-xephyr.sh: Xephyr executable is missing in PATH"
    exit 1
fi

if [ -d .stack-work ]
then
    echo "test-xephyt.sh: Stack build detected"
    BIN_PATH=$(stack path --dist-dir)/build/xmonad-ng
elif [ -d dist ]; then
    echo "test-xephyr.sh: Cabal build detected"
    BIN_PATH=$(find dist/ -type f -executable -name xmonad-ng -printf '%h')
else
    echo "test-xephyr.sh: You need to build xmonad-ng first, see README for instructions"
    exit 1
fi

RAW_BIN="$BIN_PATH/xmonad-ng"
ARCH_BIN="$BIN_PATH/xmonad-ng-$ARCH-$OS"

cp -p "$RAW_BIN" "$ARCH_BIN"

XMONAD_CONFIG_DIR="$(pwd)/state/config"
XMONAD_CACHE_DIR="$(pwd)/state/cache"
XMONAD_DATA_DIR="$(pwd)/state/data"
export XMONAD_CONFIG_DIR XMONAD_CACHE_DIR XMONAD_DATA_DIR

mkdir -p "$XMONAD_CONFIG_DIR" "$XMONAD_CACHE_DIR" "$XMONAD_DATA_DIR"
echo "test-xephyr.sh: State files will be stored in $(pwd)/state"

SCREEN_COUNTER=0
SCREEN_OPTS=""
X_OFFSET_CURRENT="0"
X_OFFSET_ADD=$(echo "$SCREEN_SIZE" | cut -dx -f1)

while expr "$SCREEN_COUNTER" "<" "$SCREENS"
do
    SCREEN_OPTS="$SCREEN_OPTS -origin ${X_OFFSET_CURRENT},0 -screen ${SCREEN_SIZE}+${X_OFFSET_CURRENT}"
    SCREEN_COUNTER=$(("$SCREEN_COUNTER" + 1))
    X_OFFSET_CURRENT=$(("$X_OFFSET_CURRENT" + "$X_OFFSET_ADD"))
done

(
    echo "test-xephyr.sh: Launching Xephyr process"
    # shellcheck disable=SC2086
    Xephyr $SCREEN_OPTS \
        +extension RANDR \
        +xinerama \
        -ac \
        -br \
        -reset \
        -softCursor \
        -terminate \
        -verbosity 10 \
        ":$DISPLAY_NUMBER" &

    export DISPLAY=":$DISPLAY_NUMBER"
    echo "test-xephyr.sh: Waiting for windwos to appear..." && sleep 3

    xterm -hold xrandr &
    xterm &

    $ARCH_BIN
)
