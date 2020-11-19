let g:grammarous#languagetool_cmd = 'languagetool'
let g:grammarous#show_first_error = 0

" augroup languagetool
"   au!
"   au BufWritePost *.adoc GrammarousCheck
" augroup end

let g:grammarous#hooks = {}
function! g:grammarous#hooks.on_check(errs) abort
    nmap <buffer><C-n> <Plug>(grammarous-move-to-next-error)
    nmap <buffer><C-p> <Plug>(grammarous-move-to-previous-error)
    nmap <buffer><C-f> <Plug>(grammarous-fixit)
    nmap <buffer><C-r> <Plug>(grammarous-reset)
    nmap <buffer><C-d> <Plug>(grammarous-remove-error)
    nmap <buffer><C-o> <Plug>(grammarous-open-info-window)
    nmap <buffer><C-c> <Plug>(grammarous-close-info-window)
endfunction

function! g:grammarous#hooks.on_reset(errs) abort
    nunmap <buffer><C-n>
    nunmap <buffer><C-p>
    nunmap <buffer><C-f>
    nunmap <buffer><C-r>
    nunmap <buffer><C-d>
    nunmap <buffer><C-o>
    nunmap <buffer><C-c>
endfunction
