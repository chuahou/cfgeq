{
  inputs.nixpkgs.url  = "nixpkgs/nixos-21.05";

  outputs = inputs@{ self, nixpkgs, ... }:
  let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};

  in rec {
    defaultPackage.${system} =
      (pkgs.haskellPackages.callCabal2nix "cfgeq" ./. {});

    devShell.${system} =
      (pkgs.haskell.lib.overrideCabal defaultPackage.${system} (old: {
        buildTools = (old.buildTools or []) ++ (with pkgs.haskellPackages; [
          cabal-install
          haskell-language-server
        ]);
      })).env;
  };
}
