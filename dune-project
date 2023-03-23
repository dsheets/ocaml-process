(lang dune 3.0)

(name process)

(generate_opam_files false)

(source
 (github dsheets/ocaml-process))

(authors "David Sheets" "Jonathan Protzenko")

(maintainers "David Sheets <sheets@alum.mit.edu>")

(license ISC)

(package
 (name process)
 (synopsis "Easy process control in OCaml")
 (depends ocaml dune base-unix base-bytes)
 (tags
  (topics process subprocess command system)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
