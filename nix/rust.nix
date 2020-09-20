let
  sources = import ./sources.nix;
  pkgs = import sources.nixpkgs { };
  platform = pkgs.rustPlatform;
in
{
  cargo = platform.rust.cargo;
  clippy = pkgs.rustPackages.clippy;
  rls = pkgs.rls;
  rustc = platform.rust.rustc;
  rustfmt = pkgs.rustfmt;
  src = platform.rustcSrc;
}
