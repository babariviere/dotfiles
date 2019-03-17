function CocPlug()
    Plug 'neoclide/coc-neco'
    Plug 'Shougo/neoinclude.vim'
	Plug 'jsfaint/coc-neoinclude'
    Plug 'neoclide/coc.nvim', {'tag': '*', 'do': {-> coc#util#install()}}
    Plug 'Shougo/denite.nvim'
endfunction

" This function is based on one from FlatColor: https://github.com/MaxSt/FlatColor/
" Which in turn was based on one found in hemisu: https://github.com/noahfrederick/vim-hemisu/
function! s:h(group, style)
  if g:palenight_terminal_italics == 0
    if has_key(a:style, "cterm") && a:style["cterm"] == "italic"
      unlet a:style.cterm
    endif
    if has_key(a:style, "gui") && a:style["gui"] == "italic"
      unlet a:style.gui
    endif
  endif
  if g:palenight_termcolors == 16
    let l:ctermfg = (has_key(a:style, "fg") ? a:style.fg.cterm16 : "NONE")
    let l:ctermbg = (has_key(a:style, "bg") ? a:style.bg.cterm16 : "NONE")
  else
    let l:ctermfg = (has_key(a:style, "fg") ? a:style.fg.cterm : "NONE")
    let l:ctermbg = (has_key(a:style, "bg") ? a:style.bg.cterm : "NONE")
  endif
  execute "highlight" a:group
    \ "guifg="   (has_key(a:style, "fg")    ? a:style.fg.gui   : "NONE")
    \ "guibg="   (has_key(a:style, "bg")    ? a:style.bg.gui   : "NONE")
    \ "guisp="   (has_key(a:style, "sp")    ? a:style.sp.gui   : "NONE")
    \ "gui="     (has_key(a:style, "gui")   ? a:style.gui      : "NONE")
    \ "ctermfg=" . l:ctermfg
    \ "ctermbg=" . l:ctermbg
    \ "cterm="   (has_key(a:style, "cterm") ? a:style.cterm    : "NONE")
endfunction

function CocSetup(enabled)
    if !a:enabled
        return
    endif

    let s:colors = palenight#GetColors()
    call s:h('CocErrorSign', {"fg": s:colors.red})
    call s:h('CocWarningSign', {"fg": s:colors.dark_yellow})
    call s:h('CocInfoSign', {"fg": s:colors.yellow})
    call s:h('CocHintSign', {"fg": s:colors.cyan})
    call s:h('CocHighlightText', {"bg": s:colors.visual_grey})
endfunction
