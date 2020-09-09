# gch() {
#   git checkout $(git branch --all | grep -v '* ' | fzf | tr -d [:space:])
# }
