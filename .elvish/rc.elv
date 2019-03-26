E:EDITOR = "nvim"
E:LC_ALL = "en_US.utf-8"

use epm

epm:install                                 \
    &silent-if-installed=$true              \
    github.com/tylerreckart/gondolin        \
    github.com/zzamboni/elvish-modules      \
    github.com/zzamboni/elvish-completions  \
    github.com/muesli/elvish-libs           \
    github.com/iwoloschin/elvish-packages	\
	github.com/xiaq/edit.elv

# Theme
use github.com/tylerreckart/gondolin/gondolin

# Aliases
use github.com/zzamboni/elvish-modules/alias

alias:new ls   e:ls --color=auto
alias:new l    e:ls --color=auto -l
alias:new la   e:ls --color=auto -la
alias:new grep e:grep --color=auto

# Completions
use github.com/zzamboni/elvish-completions/vcsh
use github.com/zzamboni/elvish-completions/cd
use github.com/zzamboni/elvish-completions/ssh
use github.com/zzamboni/elvish-completions/builtins
use github.com/zzamboni/elvish-completions/comp
use github.com/zzamboni/elvish-completions/git
git:init

# Bang bang
use github.com/zzamboni/elvish-modules/bang-bang

# Smart matcher for completions
use github.com/xiaq/edit.elv/smart-matcher
smart-matcher:apply

# Dir
use github.com/zzamboni/elvish-modules/dir
alias:new cd  &use=[github.com/zzamboni/elvish-modules/dir] dir:cd
alias:new cdb &use=[github.com/zzamboni/elvish-modules/dir] dir:cdb

edit:insert:binding[Alt-i] = $dir:history-chooser~
edit:insert:binding[Alt-b] = $dir:left-small-word-or-prev-dir~
edit:insert:binding[Alt-f] = $dir:right-small-word-or-next-dir~

use github.com/zzamboni/elvish-modules/util

use github.com/muesli/elvish-libs/git

#use github.com/iwoloschin/elvish-packages/update
#update:curl-timeout = 3
#update:check-commit &verbose

-exports- = (alias:export)
