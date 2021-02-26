(module theme.horizon
  {require {theme lib.theme}})

(local syntax {:apricot "#f09483"
               :cranberry "#e95678"
               :lavender "#b877db"
               :rosebud "#fab795"
               :turquoise "#25b0bc"})

(local ui
  {:accent "#2e303e"
   :accentAlt "#6c6f93"
   :secondaryAccent "#e9436d"
   :secondaryAccentAlt "#e95378"
   :tertiaryAccent "#fab38e"
   :background "#1c1e26"
   :backgroundAlt "#232530"
   :lightText "#d5d8da"
   :modified "#21bfc2"
   :negative "#f43e5c"
   :positive "#09f7a0"
   :shadow "#16161C"
   :warning "#27d797"})

(local ansi {:bright {:red "#ec6a88"}})

; TODO: documentation following this: https://github.com/Th3Whit3Wolf/one-nvim/blob/main/lua/one-nvim.lua
(theme.apply
  {
   :ColorColumn {:bg ui.accent}
   :Comment {:attrs "italic" :fg ui.accentAlt}
   :Constant {:fg syntax.apricot}
   :Define {:fg syntax.lavender}
   :Delimiter {:fg ui.lightText}
   :Directory {:fg syntax.cranberry}
   :EndOfBuffer {:fg ui.accent}
   :Error {:fg ansi.bright.red}
   :ErrorMsg {:fg ansi.bright.red}
   :FoldColumn {:fg ui.accentAlt}
   :Folded {:fg ui.accentAlt}
   :Function {:fg syntax.turquoise}
   :Identifier {:fg syntax.cranberry}
   :Include {:fg syntax.lavender}
   :LineNr {:fg ui.accentAlt :bg ui.background}
   :MatchParen {:fg ui.background :bg syntax.cranberry}
   :MoreMsg {:fg ui.positive}
   :NonText {:fg ui.accent}
   :Normal {:fg ui.lightText :bg ui.background}
   :Operator {:fg ui.lightText}
   :PreProc {:fg ui.positive}
   :Question {:fg ui.positive}
   :QuickFixLine {:fg ui.background :bg ui.accentAlt}
   :Search {:fg ui.background :bg syntax.apricot}
   :SignColumn {:fg ui.accentAlt}
   :Special {:fg syntax.cranberry}
   :SpecialKey {:fg syntax.cranberry :bg ui.accent}
   :Statement {:fg syntax.lavender}
   :StorageClass {:fg syntax.turquoise :attrs "italic"}
   :String {:fg syntax.rosebud}
   :Structure {:fg syntax.turquoise}
   :Title {:fg syntax.turquoise :attrs "bold"}
   :Todo {:fg ui.positive :attrs ["bold" "inverse"]}
   :Type {:fg syntax.cranberry}
   :Underlined {:fg syntax.turquoise :attrs "underline"}
   :VertSplit {:fg ui.backgroundAlt :bg ui.backgroundAlt}
   :Visual {:bg "#4b5166"}
   :WarningMsg {:fg ui.warning}
   :WildMenu {:fg ui.shadow :bg ui.warning}

   ; ;; TreeSitter
   ; TSError              { :link :Error} ;; For syntax/parser errors.
   ; TSPunctDelimiter     { :link :Delimiter} ;; For delimiters ie: `.`
   ; TSPunctBracket       {} ;; For brackets and parens.
   ; TSPunctSpecial       {} ;; For special punctutation that does not fall in the catagories before.
   ; TSConstant           {} ;; For constants
   ; TSConstBuiltin       {} ;; For constant that are built in the language: `nil` in Lua.
   ; TSConstMacro         {} ;; For constants that are defined by macros: `NULL` in C.
   ; TSString             {} ;; For strings.
   ; TSStringRegex        {} ;; For regexes.
   ; TSStringEscape       {} ;; For escape characters within a string.
   ; TSCharacter          {} ;; For characters.
   ; TSNumber             {} ;; For integers.
   ; TSBoolean            {} ;; For booleans.
   ; TSFloat              {} ;; For floats.
   ; TSFunction           {} ;; For function (calls and definitions).
   ; TSFuncBuiltin        {} ;; For builtin functions: `table.insert` in Lua.
   ; TSFuncMacro          {} ;; For macro defined fuctions (calls and definitions): each `macro_rules` in Rust.
   ; TSParameter          {} ;; For parameters of a function.
   ; TSParameterReference {} ;; For references to parameters of a function.
   ; TSMethod             {} ;; For method calls and definitions.
   ; TSField              {} ;; For fields.
   ; TSProperty           {} ;; Same as `TSField`.
   ; TSConstructor        {} ;; For constructor calls and definitions: `{ }` in Lua, and Java constructors.
   ; TSConditional        {} ;; For keywords related to conditionnals.
   ; TSRepeat             {} ;; For keywords related to loops.
   ; TSLabel              {} ;; For labels: `label:` in C and `:label:` in Lua.
   ; TSOperator           {} ;; For any operator: `+`, but also `->` and `*` in C.
   ; TSKeyword            {} ;; For keywords that don't fall in previous categories.
   ; TSKeywordFunction    {} ;; For keywords used to define a fuction.
   ; TSException          {} ;; For exception related keywords.
   ; TSType               {} ;; For types.
   ; TSTypeBuiltin        {} ;; For builtin types (you guessed it, right ?).
   ; TSNamespace          {} ;; For identifiers referring to modules and namespaces.
   ; TSInclude            {} ;; For includes: `#include` in C, `use` or `extern crate` in Rust, or `require` in Lua.
   ; TSAnnotation         {} ;; For C++/Dart attributes, annotations that can be attached to the code to denote some kind of meta information.
   ; TSText               {} ;; For strings considered text in a markup language.
   ; TSStrong             {} ;; For text to be represented with strong.
   ; TSEmphasis           {} ;; For text to be represented with emphasis.
   ; TSUnderline          {} ;; For text to be represented with an underline.
   ; TSTitle              {:link :Title} ;; Text that is part of a title.
   ; TSLiteral            {} ;; Literal text.
   ; TSURI                {} ;; Any URI like a link or email.
   ; TSVariable           {} ;; Any variable name that does not have another highlight.
   ; TSVariableBuiltin    {} ;; Variable names that are defined by the languages, like `this` or `self`.)

   ;; Cursor
   :CursorColumn {:bg ui.accent}
   :CursorLine {:bg ui.accent}
   :Cursor {:fg ui.accentAlt :bg ui.accentAlt}
   :CursorLineNr {:fg ui.lightText}

   ;; Diff
   :DiffAdd {:fg ui.positive}
   :DiffChange {:fg ui.modified}
   :DiffDelete {:fg ui.negative}
   :DiffText {:fg ui.background :bg "#5b7881"} ;; TODO: better highlight
   :diffAdded {:link :DiffAdd}
   :diffRemoved {:link :DiffDelete}

   ;; Pmenu
   :Pmenu {:bg ui.accent}
   :PmenuSbar {:bg ui.backgroundAlt}
   :PmenuSel {:fg ui.lightText :bg ui.accentAlt}
   :PmenuThumb {:bg ui.background}

   ;; Spell
   :SpellBad {:sp ansi.bright.red :attrs "undercurl"}
   :SpellCap {:sp syntax.turquoise :attrs "undercurl"}
   :SpellLocal {:sp syntax.cranberry :attrs "undercurl"}
   :SpellRare {:sp syntax.apricot :attrs "undercurl"}

   ;; Status Line
   :StatusLine {:fg ui.lightText :bg ui.background}
   :StatusLineTerm {:link :StatusLine}
   :StatusLineNC {:fg ui.lightText :bg ui.backgroundAlt}
   :StatusLineTermNC {:link :StatusLineNC}

   ;; Tab Line
   :TabLine {:fg ui.lightText :bg ui.background}
   :TabLineFill {:fg ui.lightText :bg ui.background :attrs "reverse"}
   :TabLineSel {:fg ui.lightText :bg ui.background}

   ;; CSS
   :cssBraces {:link :Delimiter}
   :cssClassName {:link :Special}
   :cssClassNameDot {:link :Normal}
   :cssPseudoClassId {:link :Special}
   :cssTagName {:link :Statement}

   ;; CSS / Sass
   :sassClass {:link :Special}

   ;; HTML
   :helpHyperTextJump {:link :Constant}
   :htmlArg {:link :Constant}
   :htmlEndTag {:link :Statement}
   :htmlTag {:link :Statement}

   ;; JSON
   ; :jsonQuote {:}

   ;; PHP
   :phpVarSelector {:link :Identifier}

   ;; Python
   :pythonFunction {:link :Title}

   ;; Ruby
   :rubyDefine {:link :Statement}
   :rubyFunction {:link :Title}
   :rubyInterpolationDelimiter {:link :String}
   :rubySharpBang {:link :Comment}
   :rubyStringDelimiter {:link :String}

   ;; Shell
   :shFunction {:link :Normal}

   ;; Vim
   :vimContinue {:link :Comment}
   :vimFuncSID {:link :vimFunction}
   :vimFuncVar {:link :Normal}
   :vimFunction {:link :Title}
   :vimGroup {:link :Statement}
   :vimHiGroup {:link :Statement}
   :vimHiTerm {:link :Identifier}
   :vimMapModKey {:link :Special}
   :vimOption {:link :Identifier}
   :vimVar {:link :Normal}

   ;; XML
   :xmlAttrib {:link :Constant}
   :xmlAttribPunct {:link :Statement}
   :xmlEndTag {:link :Statement}
   :xmlNamespace {:link :Statement}
   :xmlTag {:link :Statement}
   :xmlTagName {:link :Statement}

   ;; YAML
   :yamlKeyValueDelimiter {:link :Delimiter}

   ;; Ctrl-P
   :CtrlPPrtCursor {:link :Cursor}
   :CtrlPMatch {:link :Title}
   :CtrlPMode2 {:link :StatusLine}

   ;; Denite
   :deniteMatched {:link :Normal}
   :deniteMatchedChar {:link :Title}

   ;; Elixir
   :elixirOperator {:fg syntax.lavender}
   :elixirAlias {:fg syntax.turquoise}
   :elixirModuleDeclaration {:fg syntax.turquoise}

   ;; Javascript
   :jsArrowFunction {:link :Operator}
   :jsClassDefinition {:link :Normal}
   :jsClassFuncName {:link :Title}
   :jsExport {:link :Statement}
   :jsFuncName {:link :Title}
   :jsFutureKeys {:link :Statement}
   :jsFuncCall {:link :Normal}
   :jsGlobalObjects {:link :Statement}
   :jsModuleKeywords {:link :Statement}
   :jsModuleOperators {:link :Statement}
   :jsNull {:link :Constant}
   :jsObjectFuncName {:link :Title}
   :jsObjectKey {:link :Identifier}
   :jsSuper {:link :Statement}
   :jsTemplateBraces {:link :Special}
   :jsUndefined {:link :Constant}

   ;; Javascript Flow
   :jsFlowMaybe {:link :Normal}
   :jsFlowObject {:link :Normal}
   :jsFlowType {:link :PreProc}

   ;; GraphQL
   :graphqlName {:link :Normal}
   :graphqlOperator {:link :Normal}

   ;; Markdown
   :markdownBold {:link :Special}
   :markdownCode {:link :String}
   :markdownCodeDelimiter {:link :String}
   :markdownHeadingDelimiter {:link :Comment}
   :markdownRule {:link :Comment}

   ;; Nginx
   :ngxDirective {:link :Statement}

   ;; Plug
   :plug1 {:link :Normal}
   :plug2 {:link :Identifier}
   :plugDash {:link :Comment}
   :plugMessage {:link :Special}

   ;; Git Gutter
   :GitGutterAdd {:fg ui.positive}
   :GitGutterChange {:fg syntax.rosebud}
   :GitGutterChangeDelete {:fg syntax.rosebud}
   :GitGutterDelete {:fg ui.negative}

   ;; Signify
   :SignifySignAdd {:link :GitGutterAdd}
   :SignifySignChange {:link :GitGutterChange}
   :SignifySignChangeDelete {:link :GitGutterChangeDelete}
   :SignifySignDelete {:link :GitGutterDelete}
   :SignifySignDeleteFirstLine {:link :SignifySignDelete}

   ;; Startify
   :StartifyBracket {:link :Comment}
   :StartifyFile {:link :Identifier}
   :StartifyFooter {:link :Constant}
   :StartifyHeader {:link :Constant}
   :StartifyNumber {:link :Special}
   :StartifyPath {:link :Comment}
   :StartifySection {:link :Statement}
   :StartifySlash {:link :Comment}
   :StartifySpecial {:link :Normal}

   ;; Svss
   :svssBraces {:link :Delimiter}

   ;; Swift
   :swiftIdentifier {:link :Normal}

   ;; Typescript
   :typescriptAjaxMethods {:link :Normal}
   :typescriptBraces {:link :Normal}
   :typescriptEndColons {:link :Normal}
   :typescriptFuncKeyword {:link :Statement}
   :typescriptGlobalObjects {:link :Statement}
   :typescriptHtmlElemProperties {:link :Normal}
   :typescriptIdentifier {:link :Statement}
   :typescriptMessage {:link :Normal}
   :typescriptNull {:link :Constant}
   :typescriptParens {:link :Normal}})
