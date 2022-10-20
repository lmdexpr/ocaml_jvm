(lang dune 3.3)

(name ocaml_jvm)

(generate_opam_files true)

(source
 (github lmdexpr/ocaml_jvm))

(authors "Yuki Tajiri")

(maintainers "Yuki Tajiri <lmdexpr@gmail.com>")

(license MIT)

(package
 (name ocaml_jvm)
 (synopsis "JVM implementation")
 (depends
   (ocaml (>= 5.0.0))
   ocamlformat
   ocaml-lsp-server
   dune
   stdint))
