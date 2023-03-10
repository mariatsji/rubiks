let
  pkgs = import ./nixpkgs.nix;
  rubiks = pkgs.haskellPackages.callCabal2nix "rubiks" ./. { };
  thinner = x:
    with pkgs;
    haskell.lib.disableLibraryProfiling
    (haskell.lib.dontHaddock (haskell.lib.dontCheck x));
  rubiksThin = thinner rubiks;

in pkgs.haskell.lib.overrideCabal rubiksThin (old: {
  enableSharedExecutables = false;
  enableSharedLibraries = false;
  configureFlags = [ ];
})
