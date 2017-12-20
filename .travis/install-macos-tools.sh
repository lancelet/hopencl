#!/usr/bin/env bash

# Install Haskell stack
if ! [ -x "$(command -v stack)" ]; then
    echo "Installing Haskell stack."
    mkdir -p "$HOME/.local/bin"
    export PATH="$HOME/.local/bin:$PATH"
    curl -sSL https://get.haskellstack.org/ | sh
else
    echo "Haskell stack already installed at: $(command -v stack)"
fi
