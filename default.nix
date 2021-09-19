/*
nix-build -A recursion-schemes.components.library

nix-build -A projectCross.ghcjs.hsPkgs.recursion-schemes.components.exes.your-exe-name
nix-build -A projectCross.mingwW64.hsPkgs.recursion-schemes.components.exes.your-exe-name

*/
let
  sources = import ./nix/sources.nix {};

  haskellNix = import sources.haskellNix {};

  pkgs = import
    haskellNix.sources.nixpkgs-2009
    haskellNix.nixpkgsArgs;

in pkgs.haskell-nix.project {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "haskell-nix-project";
    src = ./.;
  };

  # Not required for `stack.yaml` based projects.
  compiler-nix-name = "ghc8107";
}
