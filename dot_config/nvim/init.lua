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
local lazypath = fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require('lazy').setup({
    'wbthomason/packer.nvim',

    -- Themes
    'phanviet/vim-monokai-pro',

    -- Languages
    'fatih/vim-go',
    'ledger/vim-ledger',
    'tsandall/vim-rego',
    {
        'hashivim/vim-terraform',
        config = function()
            vim.api.nvim_exec([[
            augroup terraform_bindings
              autocmd BufWrite *.tf TerraformFmt
            augroup end
            ]], false)
        end,
    },
    'fourjay/vim-hurl',
    'niklasl/vim-rdf',
    'cappyzawa/starlark.vim',
    'GutenYe/json5.vim',
    'google/vim-jsonnet',
    'imsnif/kdl.vim',
    'evanleck/vim-svelte',
    'jjo/vim-cue',
    'jparise/vim-graphql',
    -- Clojure support
    'tpope/vim-dispatch',
    'radenling/vim-dispatch-neovim',
    'clojure-vim/vim-jack-in',
    'Olical/conjure',

    {
        "luukvbaal/nnn.nvim",
        config = function() require("nnn").setup() end
    },

    -- General tooling
    {
        'NeogitOrg/neogit',
        dependencies = {
          "nvim-lua/plenary.nvim",         -- required
          "nvim-telescope/telescope.nvim", -- optional
          "sindrets/diffview.nvim",        -- optional
          "ibhagwan/fzf-lua",              -- optional
        },
       config = true,
    },
    'anuvyklack/hydra.nvim',
    'duane9/nvim-rg',
    {
        'numToStr/Comment.nvim',
        config = function()
            require('Comment').setup{}
        end
    },
    'simnalamburt/vim-mundo',

    -- LSP configuration
    'neovim/nvim-lspconfig',
    'hrsh7th/cmp-nvim-lsp',
    'hrsh7th/cmp-vsnip',
    'hrsh7th/vim-vsnip',
    {
        'hrsh7th/nvim-cmp',
        dependencies = {'neovim/nvim-lspconfig'},
        config = function()
            local cmp = require('cmp')
            cmp.setup {
                snippet = {
                    expand = function(args)
                      vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
                    end,
                },
                mapping = {
                    ["<C-Space>"] = cmp.mapping.complete(),
                    ["<C-j>"] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select }),
                    ["<C-k>"] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select }),
                    ["<CR>"] = cmp.mapping.confirm({select = true, behavior = cmp.ConfirmBehavior.Replace}),
                },
                sources = cmp.config.sources({
                    { name = 'nvim_lsp' },
                    { name = 'vsnip' }, -- For vsnip users.
                    { name = 'buffer' },
                }),
            };
            local capabilities = require('cmp_nvim_lsp').default_capabilities()
            local lspconfig = require('lspconfig')
            lspconfig.gopls.setup {
                capabilities = capabilities,
            }
            lspconfig.rust_analyzer.setup {
                capabilities = capabilities,
            }
            lspconfig.denols.setup {
                capabilities = capabilities,
                on_attach = on_attach,
                root_dir = lspconfig.util.root_pattern("deno.json", "deno.jsonc"),
            }

            lspconfig.tsserver.setup {
                capabilities = capabilities,
                on_attach = on_attach,
                root_dir = lspconfig.util.root_pattern("package.json"),
            }

            lspconfig.denols.setup {
                capabilities = capabilities,
                on_attach = on_attach,
                root_dir = lspconfig.util.root_pattern("deno.json", "deno.jsonc"),
            }

            lspconfig.tsserver.setup {
                capabilities = capabilities,
                on_attach = on_attach,
                root_dir = lspconfig.util.root_pattern("package.json"),
            }

            lspconfig.lua_ls.setup {}
            lspconfig.terraformls.setup {}
        end
    },

    'itchyny/lightline.vim',
    {
       'nvim-telescope/telescope.nvim',
        dependencies = {'nvim-lua/popup.nvim', 'nvim-lua/plenary.nvim'},
    },
    'phaazon/hop.nvim',
    'preservim/nerdtree',
    {
        'nvim-treesitter/nvim-treesitter',
        build = ":TSUpdate",
        config = function()
            require'nvim-treesitter.configs'.setup {
                ensure_installed = {'go'},
                highlight = {enable = true},
                indent = {enable = true},
            }
        end
    },
    -- vimade dims inactive buffers down so that active buffers are easier
    -- to recognize
    'TaDaa/vimade',
})

require('pin-github-action').setup()



-- vim.o.completeopt = 'menuone,noselect'
-- require('compe').setup {
--     enabled = true;
--     autocomplete = true;
--     source = {
--         nvim_lsp = true;
--         nvim_lua = true;
--         omni = false;
--     };
-- };

-- ledger configuration
-- In order to get proper account completion I have to specify a custom command
-- pointing to my main ledger file.
vim.g.ledger_accounts_cmd = 'ledger -f /Users/zerok/Documents/finances/ledger/index.ledger accounts'
vim.g.svelte_indent_script = 0

-- }}}
-- Theme {{{
vim.cmd [[colorscheme monokai_pro]]
vim.o.background = "dark"
-- }}}
-- Keymappings {{{
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

vim.keymap.set("i", "jk", [[<esc>]])
vim.keymap.set("v", "<leader>c", [["+y]])
vim.keymap.set("n", "<leader>b", [[:Telescope buffers<cr>]])
vim.keymap.set("n", "<leader>B", [[:b#<cr>]])
vim.keymap.set("n", "<leader>f", [[:Telescope find_files<cr>]])
vim.keymap.set("n", "<leader>g", [[:Neogit<cr>]])
vim.keymap.set("n", "<leader><leader>", [[:HopChar1<cr>]])
vim.keymap.set("n", "<space>", [[<nop>]])
vim.keymap.set("n", "<leader>i", vim.lsp.buf.hover)
-- }}}

vim.api.nvim_exec([[
augroup markdown_bindings
  autocmd BufRead *.md nmap <buffer> <silent> <leader>p :!open -a "Marked 2" %:p<CR>
  autocmd BufRead *.mdx setf markdown
augroup end
]], false)

local ft = require('Comment.ft')
ft.set('hurl', '#%s')

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
