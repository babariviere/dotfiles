function dotfiles -w git
    git --git-dir=$HOME/dotfiles --work-tree=$HOME $argv
end
