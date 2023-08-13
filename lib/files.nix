{...}: {
  # To use with system.userActivationScripts.
  linkIfNotExist = path: pathToLink: ''
    if ! [ -d ${path} ]; then
        ln -s ${pathToLink} ${path}
    fi
  '';
}
