(lang dune 3.16)

(name FP2)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Dmitrii Andriianov")

(maintainers "Dmitrii Andriianov")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name FP2)
 (synopsis "FP lab 2")
 (description "RBSet implementation")
 (depends ocaml dune base (ounit2 :with-test)))

