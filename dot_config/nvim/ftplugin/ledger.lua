vim.opt.wrap = false

local ledger_toggle_cleared = function(opts)
    -- If we are in a line, that starts with a date, we can stop searching,
    -- otherwise scroll up
    local pos = vim.api.nvim_win_get_cursor(0)
    if string.match(vim.api.nvim_get_current_line(), "^%d")  == fail then
        vim.cmd("nohlsearch")
        vim.cmd("?^[0-9]")
    end
    local line = vim.api.nvim_get_current_line()
    local line_elems = vim.split(line, " ", {})
    local prefix = line_elems[1]
    prefix = string.gsub(prefix, "/", "\\/")
    if line_elems[2] == "*" then
        local repl_string = "s/^" .. prefix .. " \\*/" .. prefix .. "/"
        vim.cmd(repl_string)
    else
        local repl_string = "s/^" .. prefix .. " /" .. prefix .. " * /"
        vim.cmd(repl_string)
    end
    vim.api.nvim_win_set_cursor(0, pos)
end

vim.api.nvim_buf_create_user_command(0, "LedgerToggleCleared", function(opts) ledger_toggle_cleared(opts) end, {})
vim.keymap.set("n", "<localleader>c", [[:LedgerToggleCleared<cr>]])

-- hi! link ledgerTransactionDate DraculaYellow
-- hi! link ledgerTransactionConfirmed DraculaComment
