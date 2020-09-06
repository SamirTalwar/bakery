let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  rust = import ./nix/rust.nix;
  smoke = (import sources.smoke {}).smoke;
in
pkgs.mkShell {
  name = "bakery";

  buildInputs = [
    rust.cargo
    rust.rls
    rust.rustc
    rust.rustfmt
    smoke
    pkgs.niv
  ] ++ (if pkgs.stdenv.isDarwin then [pkgs.darwin.apple_sdk.frameworks.Security] else []);
}
