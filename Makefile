.PHONY: deps
deps: ## Install development dependencies
	opam install -y dune-release merlin ocamlformat utop
	OPAMSOLVERTIMEOUT=240 opam install --deps-only --with-test --with-doc -y .

.PHONY: create_switch
create_switch:
	opam switch create . --no-install

.PHONY: switch
switch: create_switch deps ## Create an opam switch and install development dependencies

.PHONY: format
format: ## Format the codebase with ocamlformat
	opam exec -- dune build --root . --auto-promote @fmt

.PHONY: build
build: ## Build the project, including non installable libraries and executables
	opam exec -- dune build --root .

.PHONY: clean
clean: ## Clean build artifacts and other generated files
	opam exec -- dune clean --root .

.PHONY: test
test:
	opam exec -- dune test

.PHONY: doc
doc: ## Generate odoc documentation
	opam exec -- dune build --root . @doc
