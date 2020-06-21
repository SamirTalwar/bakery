let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  smoke = (import sources.smoke {}).smoke;
in
with pkgs;
mkShell {
  name = "bakery";

  buildInputs = [
    cargo
    rustc
    rustfmt
    smoke
  ] ++ (if stdenv.isDarwin then [darwin.apple_sdk.frameworks.Security] else []);
}
