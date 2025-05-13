{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let nixpkgs = inputs.nixpkgs.legacyPackages.${system};
      in {
        devShells.default = nixpkgs.mkShell {
          buildInputs = [
            nixpkgs.ghc
            nixpkgs.haskell-language-server
            (nixpkgs.haskell.lib.justStaticExecutables
              nixpkgs.haskellPackages.cabal-fmt)
            nixpkgs.cabal-install
          ];
        };
      });
}
