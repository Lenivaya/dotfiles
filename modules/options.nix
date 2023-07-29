{
  config,
  options,
  lib,
  home-manager,
  ...
}:
with lib;
with lib.my; {
  options = with types; {
    user = mkOpt attrs {};

    dotfiles = let
      t = either str path;
    in {
      dir = mkOpt t (findFirst pathExists (toString ../.)
        [
          "${config.user.home}/.config/dotfiles"
          "/etc/dotfiles"
        ]);
      binDir = mkOpt t "${config.dotfiles.dir}/bin";
      configDir = mkOpt t "${config.dotfiles.dir}/config";
      modulesDir = mkOpt t "${config.dotfiles.dir}/modules";
      themesDir = mkOpt t "${config.dotfiles.modulesDir}/themes";
    };

    home = {
      file = mkOpt' attrs {} "Files to place directly in $HOME";
      configFile = mkOpt' attrs {} "Files to place in $XDG_CONFIG_HOME";
      dataFile = mkOpt' attrs {} "Files to place in $XDG_DATA_HOME";

      programs = mkOpt' attrs {} "user programs";
      services = mkOpt' attrs {} "user services";
      packages = mkOpt' (listOf package) [] "user packages";

      activation = mkOpt' attrs {} "Activation scripts";
    };

    env = mkOption {
      type = attrsOf (oneOf [str path (listOf (either str path))]);
      apply = mapAttrs (_n: v:
        if isList v
        then concatMapStringsSep ":" toString v
        else (toString v));
      default = {};
      description = "TODO";
    };
  };

  config = {
    user = let
      user = builtins.getEnv "USER";
      name =
        if elem user ["" "root"]
        then "leniviy"
        else user;
    in {
      inherit name;
      description = "The primary user account";
      extraGroups = [
        "wheel"
        "adbuser"
        "networkmanager"
        "video"
        "input"
        "uinput"
        "systemd-journal"
      ];
      isNormalUser = true;
      home = "/home/${name}";
      uid = 1000;
    };

    # Install user packages to /etc/profiles instead. Necessary for
    # nixos-rebuild build-vm to work.
    home-manager = {
      useUserPackages = true;

      # I only need a subset of home-manager's capabilities. That is, access to
      # its home.file, home.xdg.configFile and home.xdg.dataFile so I can deploy
      # files easily to my $HOME, but 'home-manager.users.leniviy.home.file.*'
      # is much too long and harder to maintain, so I've made aliases in:
      #
      #   home.file        ->  home-manager.users.leniviy.home.file
      #   home.configFile  ->  home-manager.users.leniviy.home.xdg.configFile
      #   home.dataFile    ->  home-manager.users.leniviy.home.xdg.dataFile
      #   home.programs    ->  home-manager.users.leniviy.home.programs
      #   home.services    ->  home-manager.users.leniviy.home.services
      users.${config.user.name} = {
        home = {
          file = mkAliasDefinitions options.home.file;
          packages = mkAliasDefinitions options.home.packages;
          activation = mkAliasDefinitions options.home.activation;

          # Necessary for home-manager to work with flakes, otherwise it will
          # look for a nixpkgs channel.
          inherit (config.system) stateVersion;
        };
        xdg = {
          configFile = mkAliasDefinitions options.home.configFile;
          dataFile = mkAliasDefinitions options.home.dataFile;
        };

        programs = mkAliasDefinitions options.home.programs;
        services = mkAliasDefinitions options.home.services;
      };
    };

    users.users.${config.user.name} = mkAliasDefinitions options.user;

    nix.settings = let
      users = ["root" config.user.name];
    in {
      trusted-users = users;
      allowed-users = users;
    };

    # must already begin with pre-existing PATH. Also, can't use binDir here,
    # because it contains a nix store path.
    env.PATH = ["$DOTFILES_BIN" "$XDG_BIN_HOME" "$PATH"];

    environment.extraInit =
      concatStringsSep "\n"
      (mapAttrsToList (n: v: ''export ${n}="${v}"'') config.env);
  };
}
