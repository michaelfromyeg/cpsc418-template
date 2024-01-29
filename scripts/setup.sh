#!/bin/bash

# Setup the course repoisotory. Assumes a MacOS or Linux environment with Homebrew (or Linuxbrew) available.
# A Windows version is welcomed as a contribution.

# USAGE: `bash scripts/setup.sh` (i.e., from repository root)

echo "Setting up CS418 course repository..."
echo "Installing course library..."

wget http://www.ugrad.cs.ubc.ca/~cs418/resources/erl/erl.tgz

tar -xvzf erl.tgz

mkdir -p lib
mv *.erl lib
mv *.beam lib

rm erl.tgz

echo "Installing other dependencies..."

if ! [ -x "$(command -v brew)" ]; then
    echo "Error: brew is not installed. Please install brew and try again."
    exit 1
fi

brew install erlang

echo "Overwriting erl command..."
CUSTOM_ERL="
function erl {
    /usr/bin/erl erl -eval 'code:add_path("./lib")' "$@"
}
"

if grep -q "$CUSTOM_ERL" ~/.bashrc; then
    echo "Custom erl function already exists in ~/.bashrc; skipping"
else
    echo "$CUSTOM_ERL" >> ~/.bashrc
fi

brew install rebar3

