{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable-small";

    flake-utils.url = "github:numtide/flake-utils";

  };

  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
  let overlay = final: prev: {
    haskellPackages = prev.haskellPackages.extend (
       self: super: {
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

      idea-properties = pkgs.writeText "idea.properties" ''
        idea.config.path=./.IdeaIC/config
        idea.system.path=./.IdeaIC/system
        idea.plugins.path=./.IdeaIC/plugins
        idea.log.path=./.IdeaIC/log
        idea.fatal.error.notification=enabled
      '';

   in {
    overlays = overlay;
    packages.default = pkgs.haskellPackages.read-src-asset;
    devShell = pkgs.mkShell {
      WEBIDE_PROPERTIES = idea-properties;
      IDEA_PROPERTIES = idea-properties;
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
