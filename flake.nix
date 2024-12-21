{
  inputs = {
    devshell.url = "github:numtide/devshell";
    flake-utils.url = "github:numtide/flake-utils";

    purescript-overlay.url = "github:thomashoneyman/purescript-overlay";
    purescript-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    {
      self,
      flake-utils,
      devshell,
      nixpkgs,
      ...
    }@inputs:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            inputs.purescript-overlay.overlays.default
            devshell.overlays.default
          ];
        };
      in
      {
        devShells.default = pkgs.devshell.mkShell {
          devshell.name = "class-scheduler";
          commands = [
            { package = "nodejs_23"; } # node & npm
            { package = "esbuild"; }
            { package = "spago-unstable"; }
            { package = "purs"; }
            { package = "purs-tidy"; }
            { package = "purescript-language-server"; }
          ];
        };
      }
    );
}
