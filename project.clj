(defproject face-morph "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [quil "2.8.0"]
                 [uncomplicate/neanderthal "0.21.0"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [net.mikera/core.matrix "0.62.0"]
                 [net.mikera/imagez "0.12.0"]]
  :main ^:skip-aot face-morph.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
