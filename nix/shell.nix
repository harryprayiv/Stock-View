{ pkgs, lib, stdenv, myEnv, ... }:

pkgs.mkShell {
  # Define your packages and tools for the shell
  buildInputs = with pkgs; [
    myEnv.beamPackages.erlang
    myEnv.beamPackages.elixir
    myEnv.nodePackages.nodejs
    postgresql_15
    git
    # Custom shell scripts
    (pkgs.writeShellScriptBin "phoenix-server" ''
      mix deps.get
      mix phx.server
    '')

    (pkgs.writeShellScriptBin "vite" ''
      npx vite --open
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
