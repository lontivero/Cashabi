# Can be run with:
# nix-build -E 'let pkgs = import <nixpkgs> { }; in pkgs.callPackage ./default.nix {dotnet-sdk = pkgs.dotnet-sdk_8;}' -A passthru.fetch-deps
{
  lib
, buildDotnetModule
, stdenv
, libunwind
, libuuid
, icu
, openssl
, zlib
, curl
, dotnet-sdk
, dotnet-runtime
}:
buildDotnetModule rec {
    inherit dotnet-sdk dotnet-runtime;

    pname = "nostrsabi";
    version = "0.0.1";
    nugetDeps = ./deps.nix; # nix build .#packages.x86_64-linux.default.passthru.fetch-deps

    src = ./..;

    projectFile = "NostrSabi.fsproj";
    executables = [ "NostrSabi" ];

    doCheck = true;

    meta = with lib; {
      homepage = "some_homepage";
      description = "The NostrSabi Client and Minter.";
      license = licenses.mit;
    };
}
