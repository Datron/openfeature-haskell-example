{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    open-feature-hs = {
      url = "github:juspay/open-feature-haskell-sdk";
      inputs.haskell-flake.follows = "haskell-flake";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-parts.follows = "flake-parts";
    };
    superposition-provider = {
      url = "github:juspay/superposition";
      inputs.haskell-flake.follows = "haskell-flake";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-parts.follows = "flake-parts";
    };
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      imports = [
        inputs.haskell-flake.flakeModule
        # Don't import open-feature-hs.flakeModule - it doesn't exist
        # Don't import superposition-provider.flakeModule - access packages directly instead
      ];
      perSystem =
        {
          config,
          pkgs,
          system,
          self',
          ...
        }:
        let
          hpkgs = pkgs.haskell.packages.ghc98;

          # Get the open-feature package from the input
          open-feature-pkg = inputs.open-feature-hs.packages.${system}.open-feature or null;

          # Get superposition packages
          superposition-core-pkg =
            inputs.superposition-provider.packages.${system}.superposition_core or null;
          superposition-haskell-pkgs = inputs.superposition-provider.packages.${system} or { };
        in
        {
          haskellProjects.default = {
            basePackages = hpkgs;

            packages =
              {
                # aeson.source = "2.2.3.0"; # Hackage version
                # hashable.source = "1.4.0.0";
                # Add open-feature as a local package if available
              }
              // pkgs.lib.optionalAttrs (open-feature-pkg != null) {
                open-feature.source = open-feature-pkg;
              };

            settings = {
              # Configure superposition_core if available
              superposition_core = pkgs.lib.optionalAttrs (superposition-core-pkg != null) {
                extraLibraries = [ superposition-core-pkg ];
              };
            };

            devShell = {
              hlsCheck.enable = false;
            };

            autoWire = [
              "packages"
              "apps"
              "checks"
            ]; # Wire all but the devShell
          };

          packages.default =
            self'.packages.haskell-openfeature-example
              or (builtins.head (builtins.attrValues config.haskellProjects.default.outputs.packages));

          devShells.default = pkgs.mkShell {
            name = "superposition openfeature dev shell";

            shellHook = pkgs.lib.optionalString (superposition-core-pkg != null) ''
              export LIBRARY_PATH=${superposition-core-pkg}/lib:''${LIBRARY_PATH:-}
              export C_INCLUDE_PATH=${superposition-core-pkg}/include:''${C_INCLUDE_PATH:-}
            '';

            inputsFrom = [
              config.haskellProjects.default.outputs.devShell
            ];

            nativeBuildInputs = pkgs.lib.optionals (superposition-core-pkg != null) [
              superposition-core-pkg
            ];
          };
        };
    };
}
