{
  description = "An experimental eCash system";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs { inherit system; config.allowUnfree = true; };
    in {
      packages = {
        default = self.packages.${system}.nostrsabi;

        nostrsabi = pkgs.callPackage ./default.nix {
          dotnet-sdk = pkgs.dotnet-sdk_8;
          dotnet-runtime = pkgs.dotnet-runtime_8;
        };
      };

      devShells = with pkgs; {
        default = mkShell {
          name = "nostrsabi-shell";
          packages = [
            dotnet-sdk_8
            nuget-to-nix
            websocat
            jetbrains.rider
          ];

          DOTNET_ROOT = "${dotnet-sdk_8}";

          shellHook = ''
            export DOTNET_CLI_TELEMETRY_OPTOUT=1
            export DOTNET_NOLOGO=1
            export GIT_TOP_LEVEL="$(${pkgs.git}/bin/git rev-parse --show-toplevel)"
            export PS1='\n\[\033[1;34m\][NostraSabi:\w]\$\[\033[0m\] '
          '';
        };
      };
    });
}
