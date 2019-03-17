function DeopletePlug()
    Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
    Plug 'Shougo/neoinclude.vim'
    Plug 'zchee/deoplete-zsh'
    Plug 'zchee/deoplete-go', { 'do': 'make' }
    Plug 'zchee/deoplete-clang'
    Plug 'autozimu/LanguageClient-neovim', {
        \ 'branch': 'next',
        \ 'do': 'bash install.sh',
        \ }
endfunction

function DeopleteSetup(enabled)
    if !a:enabled
        return
    endif
    " Deoplete
    let g:deoplete#enable_at_startup = 1
    let g:deoplete#sources#go#gocode_binary = $GOPATH.'/bin/gocode'
    let g:deoplete#sources#go#sort_class = ['package', 'func', 'type', 'var', 'const']
    let g:deoplete#sources#go#pointer = 1
    let g:deoplete#sources#go#use_cache = 1

    " Clang
    let g:deoplete#sources#clang#libclang_path = '/usr/lib/libclang.so'
    let g:deoplete#sources#clang#clang_header = '/usr/lib/clang/7.0.1/include'

    let g:LanguageClient_loadSettings = 1 " Use an absolute configuration path if you want system-wide settings
    let g:LanguageClient_settingsPath = '/home/babariviere/.config/nvim/settings.json'
    let g:LanguageClient_serverCommands = {}
    let g:LanguageClient_serverCommands.rust = ['rls']
    let g:LanguageClient_serverCommands.c = ['ccls']
    let g:LanguageClient_serverCommands.cpp = ['ccls']
    let g:LanguageClient_serverCommands.kotlin = ['/home/babariviere/src/KotlinLanguageServer/build/install/kotlin-language-server/bin/kotlin-language-server']
    let g:LanguageClient_serverCommands.dart = ['dart_language_server']
    let g:LanguageClient_serverCommands.python = ['pyls']
    "let g:LanguageClient_serverCommands.go = ['go-langserver', '-mode', 'stdio', '-gocodecompletion']
    let g:LanguageClient_hoverPreview = "Always"

    nnoremap <F5> :call LanguageClient_contextMenu()<CR>
    " Or map each action separately
    nnoremap <silent> K :call LanguageClient#textDocument_hover()<CR>
    nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
    nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<CR>
endfunction
