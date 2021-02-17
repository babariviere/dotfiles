set background=dark
highlight clear

if exists("syntax_on")
  syntax reset
end

let g:colors_name = "horizon"

lua require("colors.horizon")
