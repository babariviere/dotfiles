function CocPlug()
    Plug 'neoclide/coc-neco'
    Plug 'Shougo/neoinclude.vim'
	Plug 'jsfaint/coc-neoinclude'
    Plug 'neoclide/coc.nvim', {'tag': '*', 'do': {-> coc#util#install()}}
    Plug 'Shougo/denite.nvim'
endfunction

function CocSetup(enabled)
    if !a:enabled
        return
    endif
endfunction
