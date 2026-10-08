vim.api.nvim_create_autocmd('BufWritePost', {
  group = vim.api.nvim_create_augroup('RustCargoFmtOnSave', { clear = true }),
  buffer = 0,
  callback = function()
    local filepath = vim.fn.expand('%:p')
    if filepath == '' then
      return
    end

    -- Only run inside a Cargo project
    local cargo_toml = vim.fs.find('Cargo.toml', {
      upward = true,
      path = vim.fn.fnamemodify(filepath, ':h'),
    })
    if not cargo_toml or #cargo_toml == 0 then
      return
    end

    -- Save cursor position
    local cursor = vim.api.nvim_win_get_cursor(0)

    -- Run cargo fmt synchronously
    vim.fn.system('cargo fmt -- ' .. vim.fn.shellescape(filepath))

    if vim.v.shell_error ~= 0 then
      vim.notify('cargo fmt failed', vim.log.levels.ERROR)
      return
    end

    vim.cmd('e!')
  end,
})
