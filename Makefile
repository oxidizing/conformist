.PHONY: all test clean

build:
	opam exec -- dune build

clean:
	opam exec -- dune clean

test:
	opam exec -- dune test

doc:
	opam exec -- dune build @doc
