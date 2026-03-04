(lang dune 3.20)

(name naive-modcheck-coalg)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name naive-modcheck-coalg)
 (synopsis "A naive model checking library for coalgebras")
 (depends ocaml dune menhir core ounit2 zarith yojson))

(using menhir 3.0)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
