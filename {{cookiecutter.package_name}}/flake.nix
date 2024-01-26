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
      perSystem = nixpkgs.lib.genAttrs systems;
      overlays = [
        haskell-nix.overlay
      ];
      pkgs = perSystem (system:
        import nixpkgs {
          inherit system overlays;
          inherit (haskell-nix) config;
        }
      );
      project = perSystem (system: pkgs.${system}.haskell-nix.project {
        compiler-nix-name = "ghc964";
        src = ./.;
        shell.tools = {
          cabal = {};
          hlint = {};
          fourmolu = {};
          haskell-language-server = { src = pkgs.haskell-nix.sources."hls-2.4"; };
        };
        # Non-Haskell shell tools go here
        shell.buildInputs = with pkgs; [
          nixpkgs-fmt
        ];
      });
      flake = perSystem (system: project.${system}.flake {});
    in flake // {
      packages = perSystem (system: {
        {{cookiecutter.package_name}} = flake.${system}.packages."{{cookiecutter.package_name}}:exe:{{cookiecutter.package_name}}";
        default = self.packages.${system}.{{cookiecutter.package_name}};
      });
    }
}
