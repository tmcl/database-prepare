{
  description = "database-prepare: compile-time SQL interface libraries";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable-small";
    flake-utils.url = "github:numtide/flake-utils";
    sqlite-simple = {
      url = "github:tmcl/sqlite-simple/query-named-with";
      flake = false;
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      ...
    }@inputs:
    let
      overlay = final: prev: {
        haskellPackages = prev.haskellPackages.extend (
          self: super: {
            database-prepare-sqlite = database-prepare-sqlite self;
            database-prepare-postgresql = database-prepare-postgresql self;
            sqlite-simple =   super.sqlite-simple.overrideAttrs {
              src = inputs.sqlite-simple;
            } ;
       tmp-postgres =   final.lib.pipe (self.callCabal2nix "tmp-postgres" (prev.fetchFromGitHub {
          owner = "jfischoff";
          repo = "tmp-postgres";
          rev = "7f2467a6d6d5f6db7eed59919a6773fe006cf22b";
          sha256="dE1OQN7I4Lxy6RBdLCvm75Z9D/Hu+9G4ejV2pEtvL1A=";
        }) {}) [
          (final.haskell.lib.compose.addBuildDepend final.postgresql_17)
          (final.haskell.lib.compose.addBuildDepend final.procps)
          (final.haskell.lib.compose.dontCheck)
        ];

          }
        );
      };
      database-prepare-sqlite =
        haskellPackages: haskellPackages.callCabal2nix "database-prepare-sqlite" ./database-prepare-sqlite { };
      database-prepare-postgresql =
        haskellPackages: haskellPackages.callCabal2nix "database-prepare-postgresql" ./database-prepare-postgresql { };
      withEachSystem = flake-utils.lib.eachDefaultSystem (
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ overlay ];
          };
          myPax = [
            pkgs.haskellPackages.database-prepare-sqlite
            pkgs.haskellPackages.database-prepare-postgresql
          ];

          idea-properties = pkgs.writeText "idea.properties" ''
            idea.config.path=./.IdeaIC/config
            idea.system.path=./.IdeaIC/system
            idea.plugins.path=./.IdeaIC/plugins
            idea.log.path=./.IdeaIC/log
            idea.fatal.error.notification=enabled
          '';

        in
        {
          overlays = overlay;
          packages = {
            default = pkgs.haskellPackages.database-prepare-sqlite;
            database-prepare-sqlite = pkgs.haskellPackages.database-prepare-sqlite;
            database-prepare-postgresql = pkgs.haskellPackages.database-prepare-postgresql;
          };
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
              pkgs.nixfmt
              (pkgs.haskellPackages.ghcWithHoogle (
                haskellPax:
                builtins.filter (a: a.pname != "database-prepare-sqlite" && a.pname != "database-prepare-postgresql") (
                  pkgs.lib.lists.concatMap (a: a.getBuildInputs.haskellBuildInputs) myPax
                )
              ))
              pkgs.sqlite-interactive
              pkgs.postgresql
            ];
          };

        }
      );
    in
    withEachSystem
    // {

      overlays.default = overlay;
      export = {
        database-prepare-sqlite = database-prepare-sqlite;
        database-prepare-postgresql = database-prepare-postgresql;
      };

    };
}
