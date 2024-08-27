#!/bin/bash

# Check and install OCaml
# Ocamllex is included with OCaml
if ! command -v ocamlc &> /dev/null; then
    echo "OCaml is not installed. Installing..."
    sudo apt-get install ocaml -y 
else
    echo "OCaml is already installed."
fi

# Check and install Opam
if ! command -v opam &> /dev/null; then
    echo "Opam is not installed. Installing..."
    sudo apt-get install opam -y
    opam init -y --disable-sandboxing
    eval $(opam env)
else
    echo "Opam is already installed."
    eval $(opam env)
fi

# Check and install Ocamlfind
if ! command -v ocamlfind &> /dev/null; then
    echo "Ocamlfind is not installed. Installing..."
    opam install ocamlfind -y
else
    echo "Ocamlfind is already installed."
fi

# Function to check if a package is installed
is_installed() {
    ocamlfind list | grep -q "^$1 " && return 0 || return 1
}

# Check and install Menhir
if ! is_installed "menhir"; then
    echo "Menhir is not installed. Installing..."
    opam install menhir -y
else
    echo "Menhir is already installed."
fi

# Check and install Ppx_deriving
if ! is_installed "ppx_deriving"; then
    echo "Ppx_deriving is not installed. Installing..."
    opam install ppx_deriving -y
else
    echo "Ppx_deriving is already installed."
fi

# Check and install ppx_expect
if ! is_installed "ppx_expect"; then
    echo "ppx_expect is not installed. Installing..."
    opam install ppx_expect -y
else
    echo "ppx_expect is already installed."
fi

echo "Installation completed."
