{ pkgs, lib, stdenv, myEnv, ... }:

pkgs.mkShell {
  # Define your packages and tools for the shell
  buildInputs = with pkgs; [
    myEnv.beamPackages.erlang
    myEnv.beamPackages.elixir
    myEnv.nodePackages.nodejs
    postgresql_15
    git
    elixir
    nodejs
    postgresql

    git
    pgcli
    glibcLocales
    gnumake
    gcc
    # readline
    # openssl
    # libxml2
    # curl
    # libiconv
    # yarn

    ## Deploy tools
    # flyctl # fly.io
    # gigalixir

    # nodePackages.node2nix
    # Formatting .js file
    nodePackages.prettier

    nixpkgs-fmt
    codespell
    # dot -Tpng ecto_erd.dot -o erd.png
    graphviz
    # Custom shell scripts
    (pkgs.writeShellScriptBin "phoenix-server" ''
      mix deps.get
      mix phx.server
    '')
    (pkgs.writeShellScriptBin "new-pheonix" ''
      mix archive.install hex phx_new
      mix phx.new new_phoenix --live
      cd new_phoenix
    '')
    (pkgs.writeShellScriptBin "vite" ''
      npx vite --open
    '')
    (pkgs.writeShellScriptBin "pg-stop" ''
      pg_ctl -D $PGDATA -U postgres stop
    '')
    (pkgs.writeShellScriptBin "pg-reset" ''
      rm -rf $PGDATA
    '')
    (pkgs.writeShellScriptBin "pg-setup" ''
      ####################################################################
      # If database is not initialized (i.e., $PGDATA directory does not
      # exist), then set it up. Seems superfluous given the cleanup step
      # above, but handy when one gets to force reboot the iron.
      ####################################################################
      if ! test -d $PGDATA; then
        ######################################################
        # Init PostgreSQL
        ######################################################
        pg_ctl initdb -D  $PGDATA
        #### initdb --locale=C --encoding=UTF8 --auth-local=peer --auth-host=scram-sha-256 > /dev/null || exit
        # initdb --encoding=UTF8 --no-locale --no-instructions -U postgres
        ######################################################
        # PORT ALREADY IN USE
        ######################################################
        # If another `nix-shell` is  running with a PostgreSQL
        # instance,  the logs  will show  complaints that  the
        # default port 5432  is already in use.  Edit the line
        # below with  a different  port number,  uncomment it,
        # and try again.
        ######################################################
        if [[ "$PGPORT" ]]; then
          sed -i "s|^#port.*$|port = $PGPORT|" $PGDATA/postgresql.conf
        fi
        echo "listen_addresses = ${"'"}${"'"}" >> $PGDATA/postgresql.conf
        echo "unix_socket_directories = '$PGDATA'" >> $PGDATA/postgresql.conf
        echo "CREATE USER postgres WITH PASSWORD 'postgres' CREATEDB SUPERUSER;" | postgres --single -E postgres
      fi
    '')
    (pkgs.writeShellScriptBin "pg-start" ''
      ## # Postgres Fallback using docker
      ## docker run -e POSTGRES_USER=postgres -e POSTGRES_PASSWORD=postgres -p 5432:5432 postgres:14

      [ ! -d $PGDATA ] && pg-setup

      ####################################################################
      # Start PostgreSQL
      # ==================================================================
      # Setting all  necessary configuration  options via  `pg_ctl` (which
      # is  basically  a wrapper  around  `postgres`)  instead of  editing
      # `postgresql.conf` directly with `sed`. See docs:
      #
      # + https://www.postgresql.org/docs/current/app-pg-ctl.html
      # + https://www.postgresql.org/docs/current/app-postgres.html
      #
      # See more on the caveats at
      # https://discourse.nixos.org/t/how-to-configure-postgresql-declaratively-nixos-and-non-nixos/4063/1
      # but recapping out of paranoia:
      #
      # > use `SHOW`  commands to  check the  options because  `postgres -C`
      # > "_returns values  from postgresql.conf_" (which is  not changed by
      # > supplying  the  configuration options  on  the  command line)  and
      # > "_it does  not reflect  parameters supplied  when the  cluster was
      # > started._"
      #
      # OPTION SUMMARY
      # --------------------------------------------------------------------
      #
      #  + `unix_socket_directories`
      #
      #    > PostgreSQL  will  attempt  to create  a  pidfile  in
      #    > `/run/postgresql` by default, but it will fail as it
      #    > doesn't exist. By  changing the configuration option
      #    > below, it will get created in $PGDATA.
      #
      #   + `listen_addresses`
      #
      #     > In   tandem  with   edits   in  `pg_hba.conf`   (see
      #     > `HOST_COMMON`  below), it  configures PostgreSQL  to
      #     > allow remote connections (otherwise only `localhost`
      #     > will get  authenticated and the rest  of the traffic
      #     > discarded).
      #     >
      #     > NOTE: the  edit  to  `pga_hba.conf`  needs  to  come
      #     >       **before**  `pg_ctl  start`  (or  the  service
      #     >       needs to be restarted otherwise), because then
      #     >       the changes are not being reloaded.
      #     >
      #     > More info  on setting up and  troubleshooting remote
      #     > PosgreSQL connections (these are  all mirrors of the
      #     > same text; again, paranoia):
      #     >
      #     >   + https://stackoverflow.com/questions/24504680/connect-to-postgres-server-on-google-compute-engine
      #     >   + https://stackoverflow.com/questions/47794979/connecting-to-postgres-server-on-google-compute-engine
      #     >   + https://medium.com/scientific-breakthrough-of-the-afternoon/configure-postgresql-to-allow-remote-connections-af5a1a392a38
      #     >   + https://gist.github.com/toraritte/f8c7fe001365c50294adfe8509080201#file-configure-postgres-to-allow-remote-connection-md
      HOST_COMMON="host\s\+all\s\+all"
      sed -i "s|^$HOST_COMMON.*127.*$|host all all 0.0.0.0/0 trust|" $PGDATA/pg_hba.conf
      sed -i "s|^$HOST_COMMON.*::1.*$|host all all ::/0 trust|"      $PGDATA/pg_hba.conf
      #  + `log*`
      #
      #    > Setting up basic logging,  to see remote connections
      #    > for example.
      #    >
      #    > See the docs for more:
      #    > https://www.postgresql.org/docs/current/runtime-config-logging.html

      pg_ctl                                                  \
        -D $PGDATA                                            \
        -l $PGDATA/postgres.log                               \
        -o "-c unix_socket_directories='$PGDATA'"             \
        -o "-c listen_addresses='*'"                          \
        -o "-c log_destination='stderr'"                      \
        -o "-c logging_collector=on"                          \
        -o "-c log_directory='log'"                           \
        -o "-c log_filename='postgresql-%Y-%m-%d_%H%M%S.log'" \
        -o "-c log_min_messages=info"                         \
        -o "-c log_min_error_statement=info"                  \
        -o "-c log_connections=on"                            \
        start
    '')
    (pkgs.writeShellScriptBin "pg-console" ''
      psql --host $PGDATA -U postgres
    '')

    (pkgs.writeShellScriptBin "pg-mix-setup" ''
      # ####/################################################################
      # # Install Node.js dependencies if not done yet.
      # ####################################################################
      # if test -d "$PWD/assets/" && ! test -d "$PWD/assets/node_modules/"; then
      #   (cd assets && npm install)
      # fi
      ####################################################################
      # If $MIX_HOME doesn't exist, set it up.
      ####################################################################
      if ! test -d $MIX_HOME; then
        ######################################################
        # ...  but first,  test whether  there is  a `_backup`
        # directory. Had issues with  installing Hex on NixOS,
        # and Hex and  Phoenix can be copied  from there, just
        # in case.
        ######################################################
        if test -d "$PWD/_backup"; then
          cp -r _backup/.mix .nix-shell/
        else
          ######################################################
          # Install Hex and Phoenix via the network
          ######################################################
          yes | ${elixir}/bin/mix local.hex
          # Install Phoenix
          # yes | ${elixir}/bin/mix archive.install hex phx_new
          #TODO:Go to stable whenever it's released
          yes | ${elixir}/bin/mix archive.install hex phx_new 1.7.0-rc.0
        fi
      fi
      if test -f "mix.exs"; then
        # These are not in the  `if` section above, because of
        # the `hex` install glitch, it  could be that there is
        # already a `$MIX_HOME` folder. See 2019-08-05_0553
        ${elixir}/bin/mix deps.get
        ######################################################
        # `ecto.setup` is defined in `mix.exs` by default when
        # Phoenix  project  is  generated via  `mix  phx.new`.
        # It  does  `ecto.create`,   `ecto.migrate`,  and  run
        # `priv/seeds`.
        ######################################################
        ${elixir}/bin/mix ecto.setup
      fi
    '')

    (pkgs.writeShellScriptBin "check-formatted" ''
      # cd ${root}

      echo " > CHECKING nix formatting"
      ${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt *.nix --check
      echo " > CHECKING mix formatting"
      ${elixir}/bin/mix format --check-formatted
    '')
  ]
  ++ lib.optionals stdenv.isLinux [
    libnotify
    inotify-tools
  ]
  ++ lib.optionals stdenv.isDarwin [
    terminal-notifier
    darwin.apple_sdk.frameworks.CoreFoundation
    darwin.apple_sdk.frameworks.CoreServices
  ];

  shellHook = ''
    # limit mix to current project
    mkdir -p .nix-mix
    export MIX_HOME=$PWD/.nix-mix
    export PATH=$MIX_HOME/bin:$PATH
    export PATH=$MIX_HOME/escripts:$PATH

    # limit hex to current project
    mkdir -p .nix-hex
    export HEX_HOME=$PWD/.nix-hex
    export ERL_LIBS=$HEX_HOME/lib/erlang/lib
    export PATH=$HEX_HOME/bin:$PATH

    # limit history to current project
    export ERL_AFLAGS="-kernel shell_history enabled -kernel shell_history_path '\"$PWD/.erlang-history\"'"

    # PostgreSQL configuration
    export PGDATA="$PWD/db"
    echo "PostgreSQL data directory set to $PGDATA"
    echo "Welcome to the Elixir development shell!"
  '';
}
