{
  description = "OCaml Dev Environment for Advent Of Code";

  inputs = {
    nixpkgs.url = "https://flakehub.com/f/NixOS/nixpkgs/0.1.0.tar.gz";
    systems.url = "github:nix-systems/default";
  };

  outputs = { self, nixpkgs, systems }:
    let
      lib = nixpkgs.lib;
      eachSystem = lib.genAttrs (import systems);
    in
    {

      packages = eachSystem (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default = self.packages.${system}.hackgammon;

          hackgammon = pkgs.ocamlPackages.buildDunePackage {
            pname = "aoc2024";
            version = "0.0.1";
            duneVersion = "3";
            src = ./.;
            strictDeps = true;

            buildInputs = with pkgs.ocamlPackages; [
              angstrom
              core
              cmdliner
              fmt
              logs
              ppx_deriving
              ppx_deriving_cmdliner
            ];
          };
        });

      devShells = eachSystem (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default = pkgs.mkShell {

            packages =
              with pkgs; [
                ocaml
                ocamlformat
                fswatch
              ] ++ (with pkgs.ocamlPackages; [
                dune_3
                opam
                odoc
                utop
                ocaml-lsp
              ]);

            inputsFrom = [
              self.packages.${system}.hackgammon
            ];
          };
        });
    };
}
