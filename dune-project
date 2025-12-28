(lang dune 3.18)

(version 0.0.1)

(generate_opam_files true)

(name loom)

(source
 (github kiblitz/loom))

(license MIT)

(maintainers kiblitz)

(authors kiblitz)

(package
 (name loom)
 (synopsis
  "Short description")
 (description
  "Long description")
 (depends
  "cmdliner"
  "core"
  "core_kernel"
  ("dune"
   (>= "3.18.1"))
  ("ocaml"
   (>= "5.3.0"))
  "ppx_jane"))
