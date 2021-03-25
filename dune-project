(lang dune 2.4)
(generate_opam_files true)

(name conformist)
(version 0.4.0)

(authors "Josef Erben")
(source (github oxidizing/conformist))
(license MIT)
(maintainers "josef@oxidizing.io")
(homepage "https://github.com/oxidizing/conformist")
(bug_reports "https://github.com/oxidizing/conformist/issues")
(documentation "https://oxidizing.github.io/conformist/")

(package
 (name conformist)
 (synopsis "Conformist allows you to define schemas to decode, validate and sanitize input data declaratively")
 (description "
Conformist allows you to define schemas to decode, validate and sanitize input data declaratively.
It comes with runtime types for primitive OCaml types such as `int`, `string`, `bool` and `float` but also `Ptime.date`, optional and custom types.
Re-use business rules in validators and run it on the client side with js_of_ocaml.
Arbitrary meta data can be stored in schemas which is useful to build functionality on top of conformist.
")
 (depends
  ;; General system dependencies
  (dune (>= 2.4))
  (ocaml (>= 4.08.0))

  ;; Test
  (alcotest (and (>= 1.2.3) :with-test))
  (sexplib (and (>= v0.13.0) :with-test))))
