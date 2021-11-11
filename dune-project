(lang dune 2.8)

(generate_opam_files true)
(formatting (enabled_for ocaml))

(name jooskos-flattener)
(version dev)
(homepage "")
(authors "Filip Koprivec")
(maintainers "Filip Koprivec")

(package
 (name jooskos-flattener)
 (synopsis "")
 (depends
  str
  js_of_ocaml
  cohttp
  cohttp-lwt-jsoo
  solidity-parser
  solidity-common
  (ocaml (>= 4.10.0))
  (ocamlformat (= 0.18.0))))
