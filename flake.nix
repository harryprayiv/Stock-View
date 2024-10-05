{
  description = "budview";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    purescript-overlay = {
      url = "github:harryprayiv/purescript-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, purescript-overlay, ... }: let
    name = "budview";
    supportedSystems = [
      "aarch64-darwin"
      "x86_64-darwin"
      "x86_64-linux"
    ];

    forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);

    # Fetch the necessary repositories using fetchGit (deku repos and hyrule)
    dekuRepo = builtins.fetchGit {
      url = "https://github.com/mikesol/purescript-deku.git";
      rev = "276f48adde3d9354f61917f7e9ae2ae7b43df6b2";
      # hash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
      submodules = false;
    };

    hyruleRepo = builtins.fetchGit {
      url = "https://github.com/mikesol/purescript-hyrule.git";
      rev = "a2a32e02a0d8518d906ec5fb3192261f63667338";
      # hash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
      submodules = false;
    };

  in {
    devShell = forAllSystems (system: let
      overlays = [ purescript-overlay.overlays.default ];
      pkgs = import nixpkgs { inherit system overlays; };

      vite = pkgs.writeShellApplication {
        name = "vite";
        runtimeInputs = with pkgs; [ nodejs-slim ];
        text = ''
          vite --host 0.0.0.0 --port 3000
        '';
      };

      spagoWatch = pkgs.writeShellApplication {
        name = "spago-watch";
        runtimeInputs = with pkgs; [ entr spago-unstable ];
        text = ''find {src,test} | entr -s "spago $*" '';
      };

      dev = pkgs.writeShellApplication {
        name = "dev";
        runtimeInputs = with pkgs; [
          nodejs-slim
          spagoWatch
          vite
        ];
        text = ''
          concurrent "spago-watch build" vite
        '';
      };
    in
      pkgs.mkShell {
        inherit name;
        shellHook = ''
          echo "Available commands: dev"
        '';
        buildInputs = [
          pkgs.esbuild
          pkgs.nodejs_20
          pkgs.nixpkgs-fmt
          pkgs.purs
          pkgs.purescript-language-server
          pkgs.spago-unstable
          pkgs.git
          vite
          dev
        ];
      });

    packages = forAllSystems (system: let
      overlays = [ purescript-overlay.overlays.default ];
      pkgs = import nixpkgs { inherit system overlays; };

    in {
      budview-executable = pkgs.stdenv.mkDerivation {
        pname = "budview";
        version = "1.0.0";

        src = ./.;

        buildInputs = [ pkgs.spago-unstable pkgs.purs pkgs.nodejs-slim pkgs.git ];

        # Provide the necessary Git repositories to the build
        SPAGO_SRC = "${dekuRepo}/deku-core ${dekuRepo}/deku-dom ${dekuRepo}/deku-css ${hyruleRepo}";

        buildPhase = ''
          export HOME=$TMPDIR
          spago bundle --bundle-type app --platform node
        '';

        installPhase = ''
          mkdir -p $out/bin
          cp -r output/* $out/bin/
          echo '#!/bin/sh' > $out/bin/start-budview
          echo 'vite --host 0.0.0.0 --port 3000' >> $out/bin/start-budview
          chmod +x $out/bin/start-budview
        '';

        meta = with pkgs.lib; {
          description = "Budview Dashboard";
          license = licenses.mit;
          platforms = supportedSystems;
        };
      };
    });

    apps = forAllSystems (system: let
      overlays = [ purescript-overlay.overlays.default ];
      pkgs = import nixpkgs { inherit system overlays; };

    in {
      budview = {
        type = "app";
        program = "${self.packages.${system}.budview-executable}/bin/start-budview";
      };
    });
  };
}
