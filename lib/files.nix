_: {
  # To use with system.userActivationScripts.
  linkIfNotExist = path: pathToLink: ''
    if ! [ -e ${path} ]; then
        ln -s ${pathToLink} ${path}
    fi
  '';

  forceLink = path: pathToLink: ''
    if [ -e ${path} ]; then
      rm -rf ${path}
    fi
    ln -s ${pathToLink} ${path}
  '';

  forceLinkImpure = path: pathToLink: ''
    if [ -e ${path} ]; then
      rm -rf ${path}
    fi
    cat ${pathToLink} > ${path}
  '';

  createDirIfNotExist = path: ''
    if ! [ -d ${path} ]; then
      mkdir -p ${path}
    fi
  '';
}
