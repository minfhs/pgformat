up:
    opam install core core_unix ppx_inline_test ppx_expect
install:
    dune build --profile release
    sudo cp _build/install/default/bin/pgformat /usr/local/bin
