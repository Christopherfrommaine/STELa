let
  pkgs = import <nixpkgs> {};
in pkgs.mkShell {
  buildInputs = [
    
    pkgs.rustup

  ];

  shellHook = ''
    export RUST_LOG=debug
  '';
}
