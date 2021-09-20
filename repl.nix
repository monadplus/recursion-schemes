# $ nix repl repl.nix
# nix> pkgs.haskell-nix.<autocomplete>
{ nixpkgs ? <nixpkgs> }: rec {
  sources = import ./nix/sources.nix { };
  haskellNix = import sources.haskellNix { };
  pkgs =
    import haskellNix.sources.nixpkgs-2009 haskellNix.nixpkgsArgs;
}
