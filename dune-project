(lang dune 3.17)

(name pgformat)

(generate_opam_files true)

(source
 (github minfhs/pgformat))

(authors "Alexander Brevig <alexander.brevig@hey.com>")

(maintainers "Alexander Brevig <alexander.brevig@hey.com>")

(license MIT)

(documentation https://github.com/minfhs/pgformat)

(package
 (name pgformat)
 (synopsis "Simple utility to format postgres sql with optinionated style")
 (description "Very opinionated style")
 (depends ocaml core core_unix)
 (tags
  ("formatter" "postgres" "pretty print")))
