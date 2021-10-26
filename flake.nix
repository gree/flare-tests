{
  description = "flare-tests";

  nixConfig = {
    bash-prompt = "\[\\e[1m\\e[32mdev-flare\\e[0m:\\w\]$ ";
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self
            , nixpkgs
            , flake-utils
            , ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs {inherit system;
                                 config.allowBroken = true;
                                };
          test-sandbox = pkgs.haskell.lib.dontCheck
            (pkgs.haskellPackages.callHackageDirect {
              pkg = "test-sandbox";
              ver = "0.1.9";
              sha256 = "14vsv7crx3h0nwqy1f99q47riv70jal9izvshha7v1z15hkslmdz";
            } {});
          test-sandbox-hunit =  pkgs.haskell.lib.dontCheck
            (pkgs.haskellPackages.callHackageDirect {
              pkg = "test-sandbox-hunit";
              ver = "0.1.3";
              sha256 = "1sh12xh604ik8dxgi1h0ml5hjpks1dwa1k73k98c78p2hzl2g30h";
            } {inherit test-sandbox;});
          test-framework-sandbox = pkgs.haskell.lib.dontCheck
            (pkgs.haskellPackages.callHackage
              "test-framework-sandbox"
              "0.1.1"
              {inherit test-sandbox;});
          test-sandbox-quickcheck = pkgs.haskell.lib.dontCheck
            (pkgs.haskellPackages.callHackage
              "test-sandbox-quickcheck"
              "0.1.0"
              {inherit test-sandbox;});
          flare-tests = with pkgs.haskell.lib;  pkgs.haskellPackages.callCabal2nix "flare-tests" ./. 
            {
            inherit test-sandbox;
            inherit test-framework-sandbox;
            inherit test-sandbox-hunit;
            inherit test-sandbox-quickcheck;
          };
      in {
        # Exported packages.
        defaultPackage = flare-tests;
        packages = {
          inherit flare-tests;
        };
      }
    );
}
