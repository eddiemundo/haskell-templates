{
  description = "A very basic flake for {{cookiecutter.package_name}}";
  inputs = {
    nixos-flake.url = "git+file:///home/jon/projects/nixos-flake";
    nixpkgs.follows = "nixos-flake/nixpkgs";
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixos-flake, nixpkgs, haskell-nix, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        haskell-compiler-name = "ghc8103";
        haskell-nix-pkgs = haskell-nix.legacyPackages.${system};
        pkgs = import nixpkgs { inherit system; };
        project = haskell-nix-pkgs.haskell-nix.cabalProject' {
          src = ./.;
          compiler-nix-name = haskell-compiler-name;
        };
     in {
       # Built by `nix build .`
       defaultPackage = project.hsPkgs.{{cookiecutter.package_name}}.components.exes.{{cookiecutter.package_name}};
       
       # This is used by `nix develop .` to open a shell for use with
       devShell = project.shellFor {
         # tools = {
           # cabal = "3.2.0.0";
           # hlint = "latest";
           # haskell-language-server = "latest";
         # };
         buildInputs = with pkgs; [
           cabal-install
           haskell.packages."${haskell-compiler-name}".haskell-language-server
         ];
       };
     }
   );
}
