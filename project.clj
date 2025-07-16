(defproject processador-crm "0.1.0-SNAPSHOT"
  :description "Processador de planilhas para CRM"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [dk.ative/docjure "1.12.0"]
                 [org.clojure/data.csv "1.0.0"]]
  :main ^:skip-aot processador-crm.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
