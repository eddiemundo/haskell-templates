{
  description = "A very basic flake {{cookiecutter.package_name}}";
  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/git-hooks.nix";
  };
  outputs = { self, flake-utils, nixpkgs, haskell-nix }:
    let
      systems = [
        "x86_64-linux" 
        "x86_64-darwin" 
      ];
    in
      flake-utils.lib.eachSystem systems (system:
        let
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              nixpkgs-fmt.enable = true;
              fourmolu.enable = true;
              ormolu.settings.defaultExtensions = [ "ImportQualifiedPost" ];
            };
          };
          pkgs = import nixpkgs {
            inherit system overlays;
            inherit (haskell-nix) config;
          };
          overlays = [
            haskell-nix.overlay
            (final: prev: {
              {{cookiecutter.package_name}} = final.haskell-nix.project' {
                compiler-nix-name = "ghc982";
                src = ./.;
                shell = {
                  tools = {
                    cabal = {};
                    hlint = {};
                    fourmolu = {};
                    haskell-language-server = { src = prev.haskell-nix.sources."hls-2.9"; };
                  };
                  buildInputs = with pkgs; [
                    pre-commit-check.enabledPackages
                  ];
                  shellHook = ''
                    ${pre-commit-check.shellHook}
                  '';
                };
                inputMap = {};
              };
            })
          ];
          flake = pkgs.{{cookiecutter.package_name}}.flake {};
        in
          flake // {
            packages.default = flake.packages."{{cookiecutter.package_name}}:exe:{{cookiecutter.package_name}}";
            checks.pre-commit-check = pre-commit-check;
          }
      );
}
