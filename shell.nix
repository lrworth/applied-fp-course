{ nixpkgs ? import ./nix/nixpkgs.nix
, compiler ? "default"
}:
let
  inherit (nixpkgs) pkgs;
  # Grab our course derivation
  course = import ./. { inherit nixpkgs compiler; };

  # Override the basic derivation so we can have a more fully feature
  # environment for hacking on the course material
  courseDevEnv = (
    pkgs.haskell.lib.addBuildTools course
      [
        # Cabal
        pkgs.cabal-install

        # Include the SQLite Database application
        pkgs.sqlite

        # Include Ormolu formatter
        pkgs.haskellPackages.ormolu

        # 'ghcid' auto reloading tool
        pkgs.haskellPackages.ghcid

        # hoogle for documentation
        pkgs.haskellPackages.hoogle

        # hasktags for navigation
        pkgs.haskellPackages.hasktags
      ]
    # We don't want nix to build the thing, we want the environment so we can
    # build the thing.
  ).env;

in
  # Fly, my pretties!
courseDevEnv
