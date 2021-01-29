let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
  rust = import ./nix/rust.nix;
  smoke = (import sources.smoke { inherit pkgs; }).smoke;
in
pkgs.mkShell {
  name = "bakery";

  buildInputs = [
    (pkgs.agda.withPackages(ps: [
      ps.standard-library
    ]))
    rust.cargo
    rust.clippy
    rust.rls
    rust.rustc
    rust.rustfmt
    smoke
    pkgs.niv
    pkgs.nixpkgs-fmt

    # Testing
    pkgs.coreutils
    pkgs.hello
    pkgs.zsh
  ] ++ (if pkgs.stdenv.isDarwin then [ pkgs.darwin.apple_sdk.frameworks.Security ] else [ ]);
}
