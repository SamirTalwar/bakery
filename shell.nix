let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  rust = import ./nix/rust.nix;
  smoke = (import sources.smoke {}).smoke;
in
with pkgs;
mkShell {
  name = "bakery";

  buildInputs = [
    rust.cargo
    rust.rustc
    rust.rustfmt
    smoke
    niv
  ] ++ (if stdenv.isDarwin then [darwin.apple_sdk.frameworks.Security] else []);
}
