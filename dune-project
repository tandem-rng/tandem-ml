(lang dune 3.0)
(name tandem)
(generate_opam_files true)
(source (github tandem-rng/tandem-ml))
(license Apache-2.0)
(authors "Jessica Cox")
(maintainers "Jessica Cox")
(package
 (name tandem)
 (synopsis "Tandem8x32 for OCaml: a pseudorandom number generator fast on CPUs and GPUs alike")
 (depends
  (ocaml (>= 5.3))
  dune
  (alcotest :with-test)))
