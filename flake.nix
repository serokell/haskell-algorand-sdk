# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

{
  nixConfig = {
    flake-registry = "https://github.com/serokell/flake-registry/raw/master/flake-registry.json";
  };

  inputs = {
    flake-compat = {
      flake = false;
    };

    haskell-nix = {
      inputs.hackage.follows = "hackage";
      inputs.stackage.follows = "stackage";
    };

    hackage = {
      flake = false;
    };

    stackage = {
      flake = false;
    };

    haskell-nix-weeder = {
     flake = false;
    };
  };

  outputs = { self, nixpkgs, haskell-nix, serokell-nix, hackage, stackage, flake-compat, haskell-nix-weeder }:

    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux.extend (nixpkgs.lib.composeManyExtensions [ haskell-nix.overlay serokell-nix.overlay ]);
      lib = pkgs.lib;

      hs-package-name = "algorand-sdk";

      # invoke haskell.nix
      hs-pkgs = pkgs.haskell-nix.stackProject {
        src = pkgs.haskell-nix.haskellLib.cleanGit {
          name = hs-package-name;
          src = ./.;
        };

        # haskell.nix configuration
        modules = [{
          packages.${hs-package-name} = {
            ghcOptions = [
              # fail on warnings
              "-Werror"
              # disable optimisations, we don't need them if we don't package or deploy the executable
              "-O0"

              # for weeder: produce *.dump-hi files
              "-ddump-to-file" "-ddump-hi"
            ];

            # for weeder: collect all *.dump-hi files
            postInstall = weeder-hacks.collect-dump-hi-files;
          };

        }];
      };

      hs-pkg = hs-pkgs.${hs-package-name};

      # returns the list of all components for a package
      get-package-components = pkg:
        # library
        lib.optional (pkg ? library) pkg.library
        # haddock
        ++ lib.optional (pkg ? library) pkg.library.haddock
        # exes, tests and benchmarks
        ++ lib.attrValues pkg.exes
        ++ lib.attrValues pkg.tests
        ++ lib.attrValues pkg.benchmarks;

      # all components for the current haskell package
      all-components = get-package-components hs-pkg.components;

      # for weeder:
      weeder-hacks = import haskell-nix-weeder { inherit pkgs; };

      # nixpkgs has weeder 2, but we use weeder 1
      weeder-legacy = pkgs.haskellPackages.callHackageDirect {
       pkg = "weeder";
       ver = "1.0.9";
       sha256 = "0gfvhw7n8g2274k74g8gnv1y19alr1yig618capiyaix6i9wnmpa";
      } {};

      # a derivation which generates a script for running weeder
      weeder-script = weeder-hacks.weeder-script {
       weeder = weeder-legacy;
       hs-pkgs = hs-pkgs;
       local-packages = [
         { name = hs-package-name; subdirectory = "."; }
       ];
      };

    in {
      # nixpkgs revision pinned by this flake
      legacyPackages.x86_64-linux = pkgs;

      # derivations that we can run from CI
      checks.x86_64-linux = {
        # builds all haskell components
        build-all = all-components;
        # check trailing whitespace
        trailing-whitespace = pkgs.build.checkTrailingWhitespace ./.;
        # check licenses
        reuse-lint = pkgs.build.reuseLint ./.;
        # runs the test
        test = hs-pkg.checks.algorand-lib-test;
      };

      # script for running weeder
      weeder-script = weeder-script;
    };
}
