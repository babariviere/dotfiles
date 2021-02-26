(module lib.theme
  {require {core aniseed.core
            nvim aniseed.nvim
            str aniseed.string}})

(defn- concat-attrs [attrs]
  (if (core.table? attrs)
    (str.join "," attrs)
    attrs))

(defn- highlight-link [to-group from-group]
  (nvim.command (.. "highlight! link " to-group " " from-group)))

(defn- highlight-rules [group args]
  (var opts "")
  (set opts (.. opts " guibg=" (or args.bg "none")))
  (set opts (.. opts " guifg=" (or args.fg "none")))
  (set opts (.. opts " guisp=" (or args.sp "none")))
  (if (. args :blend)
    (set opts (.. opts " blend=" args.blend)))
  (if (. args :attrs)
    (set opts (.. opts " gui=" (concat-attrs args.attrs)))
    (set opts (.. opts " gui=none")))
  (nvim.command (.. "highlight! " group opts)))

(defn- highlight [group args]
  (if (. args :link)
    (highlight-link group args.link)
    (highlight-rules group args)))

(defn clear []
  "Clear highlighting and reset everything to default"
  (nvim.command "highlight clear"))

(defn apply [theme]
  "Apply theme to vim"
  (each [group value (pairs theme)]
    (highlight group value)))
