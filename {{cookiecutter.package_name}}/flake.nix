{
  description = "A very basic flake {{cookiecutter.package_name}}";
  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
  };
  outputs = { self, nixpkgs, haskell-nix }:
    let
      systems = [
        "x86_64-linux" 
        "x86_64-darwin" 
      ];
    in
      flake-utils.lib.eachSystem systems (system:
        let
          pkgs = import nixpkgs {
            inherit system overlays;
            inherit (haskell-nix) config;
          };
          overlays = [
            haskell-nix.overlay
            (final: prev: {
              {{cookiecutter.package_name}} = final.haskell-nix.project' {
                compiler-nix-name = "ghc964";
                src = ./.;
                shell.tools = {
                  cabal = {};
                  hlint = {};
                  fourmolu = {};
                  haskell-language-server = { src = prev.haskell-nix.sources."hls-2.6"; };
                };
                shell.buildInputs = with pkgs; [
                ];
                inputMap = {};
              };
            })
          ];
          flake = pkgs.{{cookiecutter.package_name}}.flake {};
        in
          flake // {
            packages.default = flake.packages."{{cookiecutter.package_name}}:exe:{{cookiecutter.package_name}}";
          }
      );
}
