{:user {:dependencies [[com.bhauman/rebel-readline "0.1.4"]
                       [io.aviso/pretty "0.1.37"]
                       [mvxcvi/puget "1.3.1"]
                       [expound "0.8.9"]
                       [expectations/clojure-test "1.2.1"]
                       [org.clojure/tools.namespace "1.1.0"]]
        :plugins [[lein-ancient "0.7.0"] ; check  deps
                  [lein-plz "0.4.0-SNAPSHOT"]
                  [lein-pprint "1.3.2"]  ; pretty print lein deps
                  [metosin/bat-test "0.4.4"]
                  [lein-try "0.4.3"]
                  [io.aviso/pretty "0.1.37"]
                  [cider/cider-nrepl "0.25.9"]
                  [lein-cljfmt "0.6.0"]]
        :middleware [io.aviso.lein-pretty/inject] ; better stacktrace
        :injections [(require 'io.aviso.repl)] ;colours for stack traces in the REPL
        :aliases {"rebl" ["trampoline" "run" "-m" "rebel-readline.main"]}}}
