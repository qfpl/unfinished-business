{ nixpkgs ? import ./nix/nixpkgs.nix
}:
let
  pkgs = import nixpkgs {};

  drv = import ./. {
    inherit nixpkgs;
  };

  envWithTools = pkgs.lib.overrideDerivation drv.env (old: {
    buildInputs = with pkgs.haskellPackages; old.buildInputs ++ [
      cabal-install ghcid
    ];
  });
in
  if pkgs.lib.inNixShell then envWithTools else drv
