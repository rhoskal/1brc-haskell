let
  sources = import ./nix/sources.nix { };
  pkgs = import sources.nixpkgs { };

  haskellDeps = ps: with ps; [ base hspec ];

  ghc = pkgs.haskell.compiler.ghc98 haskellDeps;

  inputs = [
    pkgs.gcc
    pkgs.ghc
    pkgs.ghcid
    pkgs.cabal-install
    pkgs.llvm
    pkgs.nixfmt
    pkgs.ormolu
    pkgs.haskellPackages.lsp
  ];

  hooks = ''
    mkdir -p .nix-cabal
    export CABAL_DIR=$PWD/.nix-cabal
  '';
in pkgs.stdenv.mkDerivation {
  name = "app";
  src = ./.;
  buildInputs = inputs;
  shellHook = hooks;
}
