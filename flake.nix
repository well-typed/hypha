{
  description = "hypha — agent-first Hackage/Hoogle CLI";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      eachSystem = nixpkgs.lib.genAttrs systems;
    in {
      devShells = eachSystem (system:
        let pkgs = import nixpkgs { inherit system; };
            hp = pkgs.haskell.packages.ghc96;
        in {
          default = pkgs.mkShell {
            buildInputs = [ hp.ghc hp.cabal-install hp.haskell-language-server pkgs.zlib ];
            # happy(1) decodes ghc-lib-parser's Parser.y with the locale
            # encoding; under a non-UTF-8 locale it dies on the U+2237 in GHC
            # 9.12's grammar. See the Troubleshooting section of the README.
            LANG = "C.UTF-8";
          };
        });
    };
}
