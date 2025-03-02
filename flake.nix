{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";

    flake-utils.url = "github:numtide/flake-utils";

  };

  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
  let overlay = final: prev: {
    haskellPackages = prev.haskellPackages.extend (
       self: super: {
        effectful-core = super.effectful-core_2_5_1_0;
        effectful = super.effectful_2_5_1_0;
        read-src-asset = read-src-asset self;
      }
    );
  };
    read-src-asset =  haskellPackages: haskellPackages.callCabal2nix "read-src-asset" ./read-src-asset {};
  withEachSystem = flake-utils.lib.eachDefaultSystem  (system:
  let pkgs = import nixpkgs {
    inherit system ;
    overlays = [overlay];
    };
      myPax = [(pkgs.haskellPackages.read-src-asset)];

   in {
    overlays = overlay;
    packages.default = nixpkgs.legacyPackages.x86_64-linux.haskellPackages.read-src-asset;
    devShell = pkgs.mkShell {
      WEBIDE_PROPERTIES = ./idea.properties;
      IDEA_PROPERTIES = ./idea.properties;
      packages = [
        pkgs.haskell-language-server
        pkgs.cabal-install
        pkgs.ghcid
        pkgs.ormolu
        pkgs.hlint
        pkgs.nixd
        (pkgs.haskellPackages.ghcWithHoogle
          (haskellPax: builtins.filter (a: a.pname != "read-src-asset")
          (pkgs.lib.lists.concatMap (a: a.getBuildInputs.haskellBuildInputs) myPax)))
        pkgs.sqlite-interactive
      ];
    };

  });
  in withEachSystem // {

    export = read-src-asset;



  };
}
