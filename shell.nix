let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  smoke = (import sources.smoke {}).smoke;
in
with pkgs;
mkShell {
  name = "bakery";

  buildInputs = [
    rustPlatform.rust.cargo
    rustPlatform.rust.rustc
    rustPlatform.rustcSrc
    rustfmt
    smoke
  ] ++ (if stdenv.isDarwin then [darwin.apple_sdk.frameworks.Security] else []);
}
