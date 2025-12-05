{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    superposition = {
      url = "github:juspay/superposition";
      inputs = {
        haskell-flake.follows = "haskell-flake";
        flake-parts.follows = "flake-parts";
        # superposition doesn't work with latest nixpkgs yet
        # nixpkgs.follows = "nixpkgs";
      };
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
      ];
      perSystem =
        {
          config,
          pkgs,
          system,
          ...
        }:
        {
          haskellProjects.default = {
            imports = [
              inputs.superposition.haskellFlakeProjectModules.output
            ];

            basePackages = pkgs.haskell.packages.ghc96;

            settings = {
              monad-logger-aeson = {
                broken = false;
              };
            };
            autoWire = [
              "packages"
              "apps"
              "checks"
            ]; # Wire all but the devShell
          };

          # SuperpositionSDK.jailbreak = true;
          # superposition-bindings.jailbreak = true;
          # superposition-open-feature-provider.jailbreak = true;

          devShells.default = pkgs.mkShell {
            name = "superposition openfeature dev shell";
            inputsFrom = [
              pkgs.haskell.compiler.ghc94
              config.haskellProjects.default.outputs.devShell
              inputs.superposition.devShells.${system}.haskell
            ];
          };
        };
    };
}
