{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";

    flake-utils.url = "github:numtide/flake-utils";
    nix-jetbrains-plugins = {
      url = "github:theCapypara/nix-jetbrains-plugins";
      inputs.nixpkgs.follows = "nixpkgs";
    };

  };

  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
  let overlay = final: prev: {
    haskellPackages = prev.haskellPackages.extend (
       self: super: {
        effectful-core = super.effectful-core_2_5_1_0;
        effectful = super.effectful_2_5_1_0;
      }
    );
  };
      read-src-asset = haskellPax: haskellPax.callCabal2nix "read-src-asset" ./read-src-asset {};
    jetbrainsWithPlugins = pkgs: ide: pluginsNixpkgs: pluginsExtras: (
      pkgs.jetbrains.plugins.addPlugins ide
      (pluginsNixpkgs ++ builtins.map (a: inputs.nix-jetbrains-plugins.plugins."${pkgs.system}"."${ide.pname}"."${ide.version}"."${a}") pluginsExtras)
      );
  withEachSystem = flake-utils.lib.eachSystem [flake-utils.lib.system.x86_64-linux flake-utils.lib.system.aarch64-darwin] (system:
  let pkgs = import nixpkgs {
    inherit system ;
    overlays = [overlay];
      config.allowUnfreePredicate = pkg:
        builtins.elem (pkgs.lib.getName pkg) ["webstorm" "webstorm-with-plugins"];
      };
      myPax = [(read-src-asset pkgs.haskellPackages)];

   in {
    packages.default = read-src-asset nixpkgs.legacyPackages.x86_64-linux.haskellPackages;
    devShell = pkgs.mkShell {
      WEBIDE_PROPERTIES = ./idea.properties;
      packages = [
        pkgs.haskell-language-server
        pkgs.cabal-install
        pkgs.ghcid
        pkgs.ormolu
        pkgs.hlint
        pkgs.nixd
        (pkgs.haskellPackages.ghcWithHoogle
          (haskellPax: builtins.filter (a: a.pname != "library")
          (pkgs.lib.lists.concatMap (a: a.getBuildInputs.haskellBuildInputs) myPax)))
        pkgs.sqlite-interactive
        (jetbrainsWithPlugins pkgs pkgs.jetbrains.webstorm ["ideavim" "nixidea" "github-copilot"] ["boo.fox.haskelllsp" "com.redhat.devtools.lsp4ij"])
      ];
    };

  });
  in withEachSystem // {

    export = read-src-asset;



  };
}
