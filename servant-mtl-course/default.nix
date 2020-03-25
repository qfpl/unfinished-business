{ nixpkgs ? import ./nix/nixpkgs.nix
}:
let
  pkgs = import nixpkgs {};
  baseHaskellPackages = pkgs.haskellPackages;

  haskellPackages = baseHaskellPackages.override {
    overrides = self: super: {
      ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
      ghcWithPackages = self.ghc.withPackages;
    };
  };
in
  pkgs.haskellPackages.callPackage ./servant-mtl-course.nix {}
