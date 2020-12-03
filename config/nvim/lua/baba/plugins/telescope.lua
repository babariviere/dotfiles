require'telescope'.setup {
  defaults = require('telescope.themes').get_dropdown {
    use_less = false,
    width = 160,
    results_height = 20
  }
}
