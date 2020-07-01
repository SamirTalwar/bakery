let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  stablePkgs = import sources."nixos-20.03" {};
  rust = import ./nix/rust.nix;
  smoke = (import sources.smoke {}).smoke;
in
pkgs.mkShell {
  name = "bakery";

  buildInputs = [
    rust.cargo
    rust.rustc
    rust.rustfmt
    stablePkgs.rustracer
    smoke
    pkgs.niv
  ] ++ (if pkgs.stdenv.isDarwin then [pkgs.darwin.apple_sdk.frameworks.Security] else []);
}
