#!/bin/bash
brew_bin=$(which brew)
if [ ! -x "${brew_bin}" ]; then
	exit 0
fi
brew install \
    antigen \
    curl \
    deno \
    direnv \
    eza \
    findutils \
    fzf \
    gh \
    go \
    gopls \
    helix \
    kubernetes-cli \
    lazygit \
    lua-language-server \
    nnn \
    neovim \
    python \
    ripgrep \
    starship \
    uv \
    zoxide \
    zsh

