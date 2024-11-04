{
  description = "A flake for cl-competitive";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    systems.url = "github:nix-systems/default";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-parts,
      systems,
      treefmt-nix,
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import systems;
      imports = [ treefmt-nix.flakeModule ];

      perSystem =
        { pkgs, ... }:
        let
          pname = "cl-competitive";
          myLib = pkgs.sbcl.buildASDFSystem {
            inherit pname;
            version = "0.0.1";
            src = ./.;
            systems = [
              pname
              "${pname}/tests"
            ];
            lispLibs = with pkgs.sbcl.pkgs; [ fiveam ];
          };
          lisp = pkgs.sbcl.withPackages (ps: [ myLib ]);
        in
        {
          devShells.default = pkgs.mkShell { packages = with pkgs; [ lisp ]; };
          apps = {
            test = {
              type = "app";
              program = pkgs.writeShellScriptBin "${pname}-test" ''
                ${lisp}/bin/sbcl --noinform --non-interactive --eval "(require :asdf)" --eval "(asdf:test-system :${pname})"
              '';
            };
          };

          treefmt = {
            projectRootFile = "flake.nix";
            programs = {
              actionlint.enable = true;
              nixfmt.enable = true;
              yamlfmt.enable = true;
            };
          };
        };
    };
}
