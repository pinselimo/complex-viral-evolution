{
  description = "Dynamic mutations with a SEPIARD model";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    primitive-simd.url = "github:pinselimo/primitive-simd/doublex20";
  };

  outputs = { self, nixpkgs, flake-utils, primitive-simd }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      haskellPackages = pkgs.haskell.packages.ghc948;
      haskDeps = hsPkgs : with hsPkgs; [
        random-fu
        mwc-random
        MonadRandom
        comonad
        statistics
        matrices
        elynx-tree
        bytestring

        # Others
        vector
        vector-algorithms
        primitive
        parallel
        containers
        unordered-containers
        extra
        deepseq
        Cabal
        universe-base
        hashable
        optparse-applicative
        parallel-io
        async

        haskell-language-server
      ] ++ [ primitive-simd.packages.${system}.default ];
      f = { mkDerivation, lib, llvm
          , base, random-fu, mwc-random, MonadRandom, comonad, statistics, matrices, vector, universe-base, async
          , elynx-tree, hashable, optparse-applicative, primitive, parallel, containers, extra, deepseq, vector-algorithms
          , hedgehog, HUnit, Cabal, Chart, Chart-cairo, bytestring, unordered-containers, parallel-io}:
        let
          dependencies = [ base random-fu mwc-random MonadRandom comonad statistics matrices universe-base async
                           elynx-tree hashable optparse-applicative vector primitive parallel containers
                           extra deepseq bytestring unordered-containers parallel-io vector-algorithms
                           primitive-simd.packages.${system}.default
                         ];
        in mkDerivation {
          pname = "DynamicMutations";
          version = "0.1.0.0";
          src = ./.;
          isLibrary = true;
          isExecutable = true;
          buildTools = [ llvm ];
          executableHaskellDepends = dependencies;
          license = "unknown";
          mainProgram = "hpc";
        };
      drv = haskellPackages.callPackage f {inherit (pkgs.llvmPackages_12) llvm;};
    in {
      packages.default = drv;

      checks.bench = pkgs.haskell.lib.doBenchmark drv;

      apps.hpc = {
        type = "app";
        program = "${drv}/bin/hpc";
      };

      apps.repro = {
        type = "app";
        program = "${drv}/bin/repro";
      };

      devShells.default = pkgs.mkShell {
        nativeBuildInputs = with pkgs; [
          # dev
          (ghc.withPackages haskDeps)
          llvmPackages_12.libllvm

          # debug
          stylish-haskell
        ];
        buildInputs = [ ];
      };
    });
}
