--[[
# Pin GitHub actions

The goal of this script is to quickly pin GitHub Actions used inside your
workflows to a specific commit.

## Installation

Put that file into your (Neo)VIM config folder inside the `lua` directory
(e.g. `~/.config/nvim/lua/`).

### VIM

Add something like this into your init file:

```
lua require('pin-github-action').setup()
```

### NeoVIM

Put this into your init.lua file:

```
require('pin-github-action').setup()
```

## Usage

If you now open a YAML file inside a `.github` folder and move to a line with a
`uses: ...` statement in it, you can run `:PinGitHubAction` to pin that action
to a commit.

--]]

local uses_pat = "^%s*-?%s*uses%s*:.*$"
local pinned_pat = "^%s*-?%s*uses%s*:%s*([^@]+)@(.+) # (v.*)$"

local notpinned_pat = "^%s*-?%s*uses%s*:%s*([%w-.]+/[%w-.]+)%s*$"
local notpinned_versioned_pat = "^%s*-?%s*uses%s*:%s*([%w-.]+/[%w-.]+)@([%w.]+)%s*$"

assert(string.match("uses: actions/create-github-app-token", notpinned_pat), "notpinned_pat does not support versionless actions")
assert(string.match("uses: actions/create-github-app-token@v1", notpinned_versioned_pat), "notpinned_pat does not support versioned actions")

local get_cmd_output = function(list)
    if vim.system ~= nil then
        local proc = vim.system(list):wait()
        return proc["stdout"]
    end
    local proc = assert(io.popen(table.concat(list, " "), "r"))
    local output = assert(proc:read("*a"))
    proc:close()
    return output
end

local get_current_buffer = function()
    if vim.api ~= nil then
        return vim.api.nvim_get_current_buf()
    end
    return vim.buffer().number
end

local get_current_line = function()
    if vim.api ~= nil then
        return vim.api.nvim_get_current_line()
    end
    local linenr = vim.window().line
	return vim.buffer()[linenr]
end

local get_tag_commit = function(repo, tag)
    local output = get_cmd_output({"gh", "api", "/repos/" .. repo .. "/commits/" .. tag, "--jq", ".sha"})
    return string.gsub(output, "%s", "")
end

local pin = function(repo, commit, tag)
    repo = string.gsub(repo, "/", "\\/")
    local sub = "s/"..repo..".*/"..repo.."@"..commit.." \\# "..tag
    if vim.cmd ~= nil then
        vim.cmd(sub)
    else
        vim.command(sub)
    end
end

local mod = {}

function mod.PinGitHubAction()
    local line = get_current_line()

    local uses_match = string.match(line, uses_pat)
    if uses_match == nil then
        print("Not version")
        return
    end

    local pinned_match = string.match(line, pinned_pat)
    if pinned_match ~= nil then
        print("Already pinned")
        return
    end

    local repo, version = string.match(line, notpinned_versioned_pat)
    if repo == nil then
        repo = string.match(line, notpinned_pat)
        if repo == nil then
            print("ERROR")
            return
        end
    end
    -- TODO: Get the latest version matching the passed version (e.g. v1.1.1 for v1)

    if version ~= nil then
        local commit = get_tag_commit(repo, version)
        pin(repo, commit, version)
    else
        local latest_tag = string.gsub(get_cmd_output({"gh", "release", "list", "--repo", repo, "--json", "isLatest,tagName", "--jq", "'.[] | select(.isLatest==true) | .tagName'"}), "%s", "")
        local commit = get_tag_commit(repo, latest_tag)
        pin(repo, commit, latest_tag)
    end
end


function mod.create_command()
    local current_buffer = get_current_buffer()
    if vim.api ~= nil then
        vim.api.nvim_buf_create_user_command(current_buffer, 'PinGitHubAction', function()
            mod.PinGitHubAction()
        end, {})
    else
        vim.command("command PinGitHubAction lua require(\"pin-github-action\").PinGitHubAction()")
    end
end

function mod.setup()
    if vim.api == nil then
        vim.command("autocmd BufRead *.yml lua require(\"pin-github-action\").create_command()")
        vim.command("autocmd BufRead *.yaml lua require(\"pin-github-action\").create_command()")
        return
    end
    vim.api.nvim_create_autocmd({"BufRead"}, {
        pattern = {"*/.github/*.yaml", "*/.github/*.yml"},
        callback = mod.create_command,
    })
end

return mod
