{
  description = "Budview - Elixir development environment with modular shell and Docker support";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";  # Ensure correct reference to flake-utils
    beam-utils = {
      url = "github:nix-giant/beam-utils";
      inputs.nixpkgs.follows = "nixpkgs";  # Follows the correct nixpkgs input
      inputs.flake-utils.follows = "flake-utils";  # Ensure it follows the correct utils reference
    };
  };

  outputs = { self, nixpkgs, flake-utils, beam-utils, ... } @ inputs:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            beam-utils.overlays.default  # Applying the beam-utils overlay
            (import ./nix/overlay.nix)  # Custom overlay
          ];
        };
      in
      {
        # Define the development shell
        devShells = {
          default = pkgs.myCallPackage ./nix/shell.nix { };
        };

        # Packages - Optional future extensions for Docker builds, releases, etc.
        packages =
          let
            release = pkgs.myCallPackage ./nix/release.nix ({ } // inputs);

            buildDockerImage = hostSystem:
              pkgs.myCallPackage ./nix/docker-image.nix ({ inherit release hostSystem; } // inputs);

            dockerImages = builtins.listToAttrs (
              map (hostSystem: {
                name = "docker-image-triggered-by-${hostSystem}";
                value = buildDockerImage hostSystem;
              }) flake-utils.lib.defaultSystems
            );
          in
          { inherit release; } // dockerImages;
      }
    );
}
