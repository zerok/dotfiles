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

-- Let's use ripgrep for grepping
vim.o.grepprg="rg --vimgrep --smart-case --hidden"

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
    use 'tsandall/vim-rego'
    use 'hashivim/vim-terraform'
    use 'fourjay/vim-hurl'
    use 'niklasl/vim-rdf'

    use {
        "luukvbaal/nnn.nvim",
        config = function() require("nnn").setup() end
    }

    -- General tooling
    use { 'TimUntersberger/neogit', requires = 'nvim-lua/plenary.nvim' }
    use { 'anuvyklack/hydra.nvim' }
    --use 'jremmen/vim-ripgrep'
    use {
        'numToStr/Comment.nvim',
        config = function()
            require('Comment').setup{}
        end
    }
    use 'simnalamburt/vim-mundo'
    use 'hrsh7th/nvim-compe'
    use 'itchyny/lightline.vim'
    use {
       'nvim-telescope/telescope.nvim',
        requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}},
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
    -- Clojure support
    use {
        'tpope/vim-dispatch'
    }
    use {
        'radenling/vim-dispatch-neovim'
    }
    use {
        'clojure-vim/vim-jack-in'
    }
    use {
        'Olical/conjure'
    }
    if packer_bootstrap then
        require('packer').sync()
    end
end)

local lspconfig = require('lspconfig')
lspconfig.gopls.setup {}
lspconfig.tsserver.setup {}
lspconfig.rust_analyzer.setup({})

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
vim.g.maplocalleader = " "

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
  autocmd BufRead *.mdx setf markdown
augroup end
]], false)

local ft = require('Comment.ft')
ft.set('hurl', '#%s')

vim.api.nvim_create_user_command(
    'Rg',
    "execute 'silent grep <args>' | copen",
    {nargs='+'}
)

local Hydra = require('hydra')
Hydra({
    name = "Window movement",
    body = "<leader>w",
    heads = {
        {'v',
            function()
                vim.api.nvim_command("vsplit")
            end,
            {desc = "Split vertically"}},
        {'h',
            function()
                vim.api.nvim_command("split")
            end,
            {desc = "Split horizontally"}},
        {'+',
            function()
                vim.api.nvim_command("resize +1")
            end,
            {desc = "Higher"}},
        {'-',
            function()
                vim.api.nvim_command("resize -1")
            end,
            {desc = "Lower"}},
        {'>',
            function()
                vim.api.nvim_command("vertical resize +1")
            end,
            {desc = "Wider"}},
        {'<',
            function()
                vim.api.nvim_command("vertical resize -1")
            end,
            {desc = "Narrower"}},
        {'q',
            nil,
            {
                desc = "Quit",
                exit = true,
            }},
    },
})
