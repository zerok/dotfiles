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
    'cappyzawa/starlark.vim',
    'GutenYe/json5.vim',
    'google/vim-jsonnet',

    'duane9/nvim-rg',
    'simnalamburt/vim-mundo',

    -- LSP configuration
    'neovim/nvim-lspconfig',
    'hrsh7th/cmp-nvim-lsp',
    {
        'hrsh7th/nvim-cmp',
        dependencies = {'neovim/nvim-lspconfig'},
        config = function()
            local cmp = require('cmp')
            cmp.setup {
                mapping = {
                    ["<C-Space>"] = cmp.mapping.complete(),
                    ["<C-j>"] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select }),
                    ["<C-k>"] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select }),
                    ["<CR>"] = cmp.mapping.confirm({select = true, behavior = cmp.ConfirmBehavior.Replace}),
                },
                sources = cmp.config.sources({
                    { name = 'nvim_lsp' },
                    { name = 'buffer' },
                }),
            };
            local capabilities = require('cmp_nvim_lsp').default_capabilities()
            local lspconfig = require('lspconfig')
            vim.lsp.config('gopls', {
                capabilities = capabilities,
            })
            vim.lsp.enable('gopls')
            vim.lsp.config('rust_analyzer', {
                capabilities = capabilities,
            })
            vim.lsp.enable('rust_analyzer')
            vim.lsp.config('denols', {
                capabilities = capabilities,
                on_attach = on_attach,
                root_dir = lspconfig.util.root_pattern("deno.json", "deno.jsonc"),
                inlay_hints = { enabled = true },
            })
            vim.lsp.enable('denols')
            vim.lsp.config('ts_ls', {
                capabilities = capabilities,
                on_attach = on_attach,
                root_dir = lspconfig.util.root_pattern("package.json"),
                inlay_hints = { enabled = true },
            })
            vim.lsp.enable('ts_ls')
            vim.lsp.enable('lua_ls')
            vim.lsp.enable('terraformls')
        end
    },

    'itchyny/lightline.vim',
    {
       'nvim-telescope/telescope.nvim',
        dependencies = {'nvim-lua/popup.nvim', 'nvim-lua/plenary.nvim'},
    },
})

require('pin-github-action').setup()

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
vim.keymap.set("n", "<leader>r", [[:Telescope lsp_references<cr>]])
-- }}}

vim.api.nvim_exec([[
augroup markdown_bindings
  autocmd BufRead *.md nmap <buffer> <silent> <leader>p :!open -a "Marked 2" %:p<CR>
  autocmd BufRead *.mdx setf markdown
augroup end
]], false)

