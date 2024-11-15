(lang dune 3.15)

(name lcalcul_interpreter)

(generate_opam_files true)
(map_workspace_root false)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license MIT)

; (documentation https://url/to/documentation)

(package
 (name lcalcul_interpreter)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune (alcotest :with-test))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
