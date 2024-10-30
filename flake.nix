{
  description = "A flake for cl-competitive";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    systems.url = "github:nix-systems/default";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, systems }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import systems;
      perSystem = { pkgs, ... }:
        let
          pname = "cl-competitive";
          lispLib = pkgs.sbcl.buildASDFSystem {
            inherit pname;
            version = "0.0.1";
            src = ./.;
            systems = [ pname "${pname}/tests" ];
            lispLibs = with pkgs.sbcl.pkgs; [ fiveam ];
          };
          lisp = pkgs.sbcl.withPackages (ps: [ lispLib ]);
        in {
          apps = {
            test = {
              type = "app";
              program = pkgs.writeShellScriptBin "${pname}-test" ''
                ${lisp}/bin/sbcl --noinform --non-interactive --eval "(require :asdf)" --eval "(asdf:test-system :${pname})"
              '';
            };
          };
        };
    };
}
