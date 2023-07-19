(lang dune 3.7)

(name lambda_calculus)

(generate_opam_files true)

(source
 (github henri1i/lambda-calculus))

(authors "henri1i")
(maintainers "henri1i")

(license LICENSE)

(package
 (name lambda_calculus)
 (synopsis "A very simple lambda calculus implementation")
 (depends ocaml dune (alcotest :with-test)))
