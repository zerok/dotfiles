function! execappend#execAndAppend()
    let l:lineno = line('.')
    let l:line = getline(l:lineno)
    if l:line =~ "\$ .*"
        let l:cmd = l:line[2:]
        " Look for the end of this code block
        let l:codeuntil = 0
        for el in range(100)
            if getline(l:lineno + el) == "```"
                let l:codeuntil = el - 1
                break
            endif
        endfor
        if l:codeuntil > 0
            execute "normal! majd" . l:codeuntil . "d`a"
        endif
        let l:output = systemlist(l:cmd)
        let l:idx = 0
        for ol in l:output
            call append(l:lineno + l:idx, ol)
            let l:idx = l:idx + 1
        endfor
    else
        echo "Current line doesn't start with a !"
    endif
endfunction
