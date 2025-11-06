(lang dune 2.0)
(name carehome)
(version 0.1.0)

(generate_opam_files true)

(package
 (name carehome)
 (synopsis "Maintenance scripts for OCaml opam packaging")
 (authors "David Allsopp" "Mark Elvers")
 (maintainers "David Allsopp <david@tarides.com>" "Mark Elvers <mark@tarides.com>")
 (homepage "https://ocaml.org")
 (license "ISC")
 (bug_reports "https://github.com/dra27/carehome/issues")
 (description "Analyze and report on patches applied to OCaml compiler packages in ocaml/opam-repository")
 (depends ocaml (dune (>= 2.0)) git-unix opam-format lwt logs fmt cohttp-lwt-unix tls-lwt dockerfile-opam ocaml-version bos eio_linux eio_main progress))
