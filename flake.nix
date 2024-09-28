{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    utils.url = "github:ursi/flake-utils";
  };

  outputs = { self, nixpkgs, utils, ... } @ inputs: let
    name = "budview";
    systems = [
      "aarch64-darwin"
      "x86_64-darwin"
      "x86_64-linux"
    ];
    pkgs = nixpkgs.legacyPackages.x86_64-linux;
  in
    utils.apply-systems
    {
      inherit inputs systems;
    }
    ({
      system,
      pkgs,
      ...
    }: let
      elixir = pkgs.elixir_1_14;  # Specify the version of Elixir you want
      nodejs = pkgs.nodejs-18_x;  # Node.js LTS version 18
      postgresql = pkgs.postgresql_15;  # PostgreSQL version 15

      # Shell applications like Phoenix server, Vite, etc.
      phoenix_server = pkgs.writeShellApplication {
        name = "phoenix-server";
        runtimeInputs = with pkgs; [ elixir nodejs postgresql ];
        text = ''
          mix deps.get
          mix phx.server
        '';
      };

      vite = pkgs.writeShellApplication {
        name = "vite";
        runtimeInputs = with pkgs; [ nodejs ];
        text = "npx vite --open";
      };

    in rec {
      devShells.default =
        pkgs.mkShell {
          inherit name;
          packages = with pkgs; [
            elixir
            nodejs
            postgresql
            git
          ];

          buildInputs = with pkgs; [
            elixir
            nodejs
            postgresql
          ];

          # Shell Hook for any environment setup
          shellHook = ''
            export PGDATA="$PWD/db"
            echo "PostgreSQL data directory set to $PGDATA"
            echo "Welcome to the Elixir development shell!"
          '';

          # Custom commands available in the shell
          shellTools = [
            phoenix_server
            vite
          ];
        };

      # Define apps like live-server if needed
      apps = {
        phoenix = {
          type = "app";
          program = "${phoenix_server}/bin/phoenix-server";
        };
      };
    });
}
