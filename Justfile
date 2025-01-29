install:
    dune build --profile release
    sudo cp _build/install/default/bin/pgformat /usr/local/bin
