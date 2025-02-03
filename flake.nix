{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }: 
  let read-src-asset = legacyPax: legacyPax.haskellPackages.callCabal2nix "read-src-asset" ./read-src-asset {};
  in {

    export = read-src-asset;

    packages.x86_64-linux.default = read-src-asset nixpkgs.legacyPackages.x86_64-linux;
    packages.aarch64-darwin.default = read-src-asset nixpkgs.legacyPackages.aarch64-darwin;


  };
}
