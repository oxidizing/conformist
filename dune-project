(lang dune 2.4)
(generate_opam_files true)

(name conformist)
(version 0.0.1)

(authors "Josef Erben")
(source (github oxidizing/conformist))
(license MIT)
(maintainers "josef@oxidizing.io")
(homepage "https://github.com/oxidizing/conformist")
(bug_reports "https://github.com/oxidizing/conformist/issues")
(documentation "https://oxidizing.github.io/conformist/")

(package
 (name conformist)
 (synopsis "TODO")
 (description "TODO.")
 (depends
  ;; General system dependencies
  (dune (>= 2.4))
  (ocaml (>= 4.08.0))

  ;; Test
  (alcotest (and (>= 1.2.3) :with-test))
  (ppx_deriving (and (>= v4.5) :with-test))))
