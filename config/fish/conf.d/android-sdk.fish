# check for common android sdk locations
for root in $ANDROID_HOME $ANDROID_SDK_ROOT $HOME/Android/Sdk $HOME/Library/Android/sdk
  set -l paths
  if test -d $root
    # the sdk root exists as a directory so let's try to add the binary directories
    # we just add the most recent of the build tools that are installed
    set -l build_tools_dirs $root/build-tools/*
    for bin in $root/cmdline-tools/latest/bin $build_tools_dirs[-1] $root/platform-tools $root/emulator 
      if test -d $bin
        set paths $paths $bin
      end
    end
    # if we added at least one path, let's add them to the real PATH, set ANDROID_HOME and finish
    if set -q paths[1]
      set -gx PATH $PATH $paths
      set -gx ANDROID_HOME $root
      break
    end
   end
end
