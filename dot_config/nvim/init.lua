-- vim: foldlevelstart=0 foldmethod=marker

-- General settings {{{
vim.o.termguicolors = true
vim.o.number = true
vim.o.relativenumber = true
vim.o.tabstop = 4
vim.o.shiftwidth = 4
vim.o.expandtab = true
vim.o.textwidth = 0
vim.o.cursorline = true

vim.api.nvim_exec([[
augroup eslint_settings
  autocmd BufRead .eslintrc setf json
augroup end
]], false)

vim.api.nvim_exec([[
augroup restore_location
    autocmd BufReadPost * if @% !~# '\.git[\/\\]COMMIT_EDITMSG$' && line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
augroup end
]], false)
-- }}}
-- Plugins {{{
local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
    packer_bootstrap = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
end
require('packer').startup(function(use)
    use 'wbthomason/packer.nvim'

    -- Languages
    use 'fatih/vim-go'
    use 'ledger/vim-ledger'
    use 'rust-lang/rust.vim'
    use 'tsandall/vim-rego'
    use 'hashivim/vim-terraform'
    use 'fourjay/vim-hurl'

    -- General tooling
    use { 'TimUntersberger/neogit', requires = 'nvim-lua/plenary.nvim' }
    --use 'jremmen/vim-ripgrep'
    use 'tpope/vim-commentary'
    use 'simnalamburt/vim-mundo'
    use 'hrsh7th/nvim-compe'
    use 'itchyny/lightline.vim'
    use {
       'nvim-telescope/telescope.nvim',
        requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}},
    }
    use {
        'kristijanhusak/orgmode.nvim',
        config = function()
            require('orgmode').setup{}
        end
    }
    use {
        'neovim/nvim-lspconfig'
    }
    use {
        'phaazon/hop.nvim'
    }
    use {
        'preservim/nerdtree'
    }
    use {
        -- vimade dims inactive buffers down so that active buffers are easier
        -- to recognize
        'TaDaa/vimade'
    }
    use {
        'evanleck/vim-svelte'
    }
    use {
        'jjo/vim-cue'
    }
    use {
        'jparise/vim-graphql'
    }
    if packer_bootstrap then
        require('packer').sync()
    end
end)

require('lspconfig').gopls.setup {}
require('lspconfig').tsserver.setup {}

vim.o.completeopt = 'menuone,noselect'
require('compe').setup {
    enabled = true;
    autocomplete = true;
    source = {
        nvim_lsp = true;
        nvim_lua = true;
        omni = false;
    };
};

-- ledger configuration
-- In order to get proper account completion I have to specify a custom command
-- pointing to my main ledger file.
vim.g.ledger_accounts_cmd = 'ledger -f /Users/zerok/Documents/finances/ledger/index.ledger accounts'
vim.g.svelte_indent_script = 0

-- }}}
-- Theme {{{
vim.cmd [[colorscheme dracula_pro_van_helsing]]
-- }}}
-- Keymappings {{{
vim.g.mapleader = " "

vim.api.nvim_set_keymap("i", "jk", [[<esc>]], {noremap = true, silent = true})
vim.api.nvim_set_keymap("v", "<leader>c", [["+y]], {noremap = true, silent = true})
vim.api.nvim_set_keymap("n", "<leader>b", [[:Telescope buffers<cr>]], {noremap = true, silent = true})
vim.api.nvim_set_keymap("n", "<leader>B", [[:b#<cr>]], {noremap = true, silent = true})
vim.api.nvim_set_keymap("n", "<leader>f", [[:Telescope find_files<cr>]], {noremap = true, silent = true})
vim.api.nvim_set_keymap("n", "<leader>g", [[:Neogit<cr>]], {noremap = true, silent = true})
vim.api.nvim_set_keymap("n", "<leader><leader>", [[:HopChar1<cr>]], {noremap = true, silent = true})
vim.api.nvim_set_keymap("n", "<space>", [[<nop>]], {noremap = true, silent = true})
-- }}}

vim.cmd [[source /Users/zerok/src/gitlab.com/zerok/datasphere.vim/plugin/datasphere.vim]]

vim.api.nvim_exec([[
augroup markdown_bindings
  autocmd BufRead *.md nmap <buffer> <silent> <leader>p :!open -a "Marked 2" %:p<CR>
augroup end
]], false)
