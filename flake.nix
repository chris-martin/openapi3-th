{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
    freckle.url = "github:freckle/flakes?dir=main";
  };
  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let nixpkgs = inputs.nixpkgs.legacyPackages.${system};
      in {
        devShells.default = nixpkgs.mkShell {
          buildInputs = [ nixpkgs.zlib ];
          nativeBuildInputs = [
            #nixpkgs.haskell.compiler.ghc910
            #(nixpkgs.haskell-language-server.override {
            #  supportedGhcVersions = [ "9101" ];
            #})
            (nixpkgs.haskell.lib.justStaticExecutables
              nixpkgs.haskellPackages.cabal-fmt)
            #nixpkgs.cabal-install
            (inputs.freckle.packages.${system}.fourmolu-default)
            (inputs.freckle.lib.${system}.haskellBundle { ghcVersion = "ghc-9-10-2"; enableHLS = true; })
          ];
        };
      });
}
