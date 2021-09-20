# More at https://input-output-hk.github.io/haskell.nix/reference/library/#shellfor
let project = import ./default.nix;
in project.shellFor {

  # packages = ps: with ps; [ aeson ];

  # Optional $ nix-shell --run "hoogle server --local"
  withHoogle = true;

  # See overlays/tools.nix for more details
  tools = {
    cabal = "3.2.0.0";
    hlint = "latest";
    haskell-language-server = "latest";
  };

  # Some you may need to get some other way.
  # buildInputs = [ (import <nixpkgs> {}).git ];

  # Select cross compilers to include.
  # crossPlatforms = ps:
  #   with ps;
  #   [
  #     ghcjs # Adds support for `js-unknown-ghcjs-cabal build` in the shell
  #     # mingwW64 # Adds support for `x86_64-W64-mingw32-cabal build` in the shell
  #   ];

  # Prevents cabal from choosing alternate plans, so that
  # *all* dependencies are provided by Nix.
  exactDeps = true;
}
