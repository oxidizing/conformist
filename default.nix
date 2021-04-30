with import <nixpkgs> { };

mkShell {
  buildInputs = [ zlib.dev zlib.out zlib zlib.all gmp gmp.dev pkgconfig openssl libev libevdev ];
  shellHook = "eval $(opam env)";
}
