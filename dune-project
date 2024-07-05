(lang dune 3.15)

(name TFhree)

(generate_opam_files true)

(using menhir 3.0)

(wrapped_executables false)

(source
 (github marcoantoniocorallo/Trusted-Fhree))

(authors "Marco Antonio Corallo")

(maintainers "Marco Antonio Corallo")

(license GPL-3.0-or-later)

(package
 (name TFhree)
 (depends ocaml dune)
)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
