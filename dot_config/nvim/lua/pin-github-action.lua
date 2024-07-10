-- First we need the content of the current line
local uses_pat = "^%s*-?%s*uses%s*:.*$"
local pinned_pat = "^%s*-?%s*uses%s*:%s*([^@]+)@(.+) # (v.*)$"
local notpinned_pat = "^%s*-?%s*uses%s*:%s*([%w-.]+/[%w-.]+)(@([%w.]+))?%s*$"
local notpinned_repo_only_pat = "^%s*-?%s*uses%s*:%s*([%w-.]+/[%w-.]+)%s*$"

local get_tag_commit = function(repo, tag)
    local commit_sha_obj = vim.system({"gh", "api", "/repos/" .. repo .. "/commits/" .. tag, "--jq", ".sha"}):wait()
    return string.gsub(commit_sha_obj["stdout"], "%s", "")
end

local pin = function(repo, commit, tag)
    repo = string.gsub(repo, "/", "\\/")
    vim.cmd("s/"..repo.."/"..repo.."@"..commit.." \\# "..tag)
end

local mod = {}

function mod.PinGitHubAction()
    local line = vim.api.nvim_get_current_line()

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

    local repo, _, version = string.match(line, notpinned_pat)
    if repo == nil then
        repo = string.match(line, notpinned_repo_only_pat)
        if repo == nil then
            print("ERROR")
            return
        end
    end

    if version ~= nil then
        local commit = get_tag_commit(repo, version)
        pin(repo, commit, version)
    else
        local releases_obj = vim.system({"gh", "release", "list", "--repo", repo, "--json", "isLatest,tagName", "--jq", ".[] | select(.isLatest==true) | .tagName"}):wait()
        local latest_tag = string.gsub(releases_obj["stdout"], "%s", "")
        local commit = get_tag_commit(repo, latest_tag)
        pin(repo, commit, latest_tag)
    end
end

function mod.setup()
    vim.api.nvim_create_user_command('PinGitHubAction', "lua require('pin-github-action').PinGitHubAction()", {})
end

return mod
