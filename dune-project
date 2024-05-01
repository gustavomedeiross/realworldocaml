(lang dune 3.7)

(name realworldocaml)

(generate_opam_files true)

(source
 (github gustavomedeiross/realworldocaml))

(authors "Gustavo Medeiros")

(maintainers "Gustavo Medeiros")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name realworldocaml)
 (synopsis "My journey through the Real World OCaml book")
 (description "My journey through the Real World OCaml book")
 (depends ocaml dune base core core_unix stdio ppx_deriving ppx_expect)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project