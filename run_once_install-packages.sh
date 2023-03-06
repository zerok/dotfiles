#!/bin/bash
brew_bin=$(which brew)
if [ ! -x "${brew_bin}" ]; then
	exit 0
fi
brew install \
    asdf \
    direnv \
    nvim \
    exa \
    zoxide \
    fzf \
    starship \
    curl \
    findutils \
    antigen \
    nnn
